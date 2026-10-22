// ---------------------------------------------------------------------------
// Cal Poly Scout, as a program you double-click.
//
// What this is NOT: a packaged copy of the app. R still does the work, this
// starts it. Bundling R would mean shipping 200 MB and a second copy of it to
// keep up to date, and the laptop that codes a match has R on it already.
//
// What it does that scout.bat cannot:
//
//   - no console window to leave open, and nothing to close by accident
//     mid-match. The window it shows is the thing you want on screen anyway:
//     the address to type into the iPads.
//   - picks a free port. 7815 is held surprisingly often, by a previous run
//     that did not exit cleanly, and the batch file's answer to that was an
//     R stack trace about createTcpServer.
//   - waits until the server actually answers before opening the browser,
//     rather than opening a tab at a port nothing is listening on yet.
//   - stops R when the window closes, including the child process R starts,
//     so "I closed it" and "it stopped" mean the same thing.
//
// Built by build\make-exe.ps1 with the C# compiler that ships with Windows.
// No toolchain to install, which is the point: this has to be rebuildable on
// the laptop that runs it, in a gym, with no internet.
// ---------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Text.RegularExpressions;
using System.Threading;
using System.Windows.Forms;
using Microsoft.Win32;

static class Scout
{
    const string AppName = "Cal Poly Scout";
    static Process R;
    static Process AppWindow;      // the chromeless window, when we opened one
    static Form Win;
    static Label Status;
    static ListBox Addresses;
    static Button OpenBtn, CopyBtn, StopBtn;
    static int Port;
    static string AppDir, LogPath;
    static readonly List<string> Urls = new List<string>();

    [STAThread]
    static void Main(string[] args)
    {
        Application.EnableVisualStyles();
        AppDir = AppFolder();
        LogPath = Path.Combine(AppDir, "scoutR.log");

        string rscript = FindRscript();
        if (rscript == null)
        {
            MessageBox.Show(
                "R is not installed on this computer, or this program cannot find it.\n\n" +
                "Install R from cran.r-project.org, then open this again.",
                AppName, MessageBoxButtons.OK, MessageBoxIcon.Warning);
            return;
        }
        if (!File.Exists(Path.Combine(AppDir, "run.R")))
        {
            MessageBox.Show(
                "This program has to sit in the scoutR folder, beside run.R.\n\n" +
                "Looked in:\n" + AppDir,
                AppName, MessageBoxButtons.OK, MessageBoxIcon.Warning);
            return;
        }

        Port = args.Length > 0 ? ParsePort(args[0]) : FreePort(7815);
        BuildWindow();
        Start(rscript);
        Application.Run(Win);
    }

    // ---------------------------------------------------------------- window

    static void BuildWindow()
    {
        Win = new Form();
        Win.Text = AppName;
        Win.ClientSize = new Size(430, 250);
        Win.FormBorderStyle = FormBorderStyle.FixedSingle;
        Win.MaximizeBox = false;
        Win.StartPosition = FormStartPosition.CenterScreen;
        Win.BackColor = Color.White;
        try { Win.Icon = Icon.ExtractAssociatedIcon(Application.ExecutablePath); } catch { }

        Label title = new Label();
        title.Text = AppName;
        title.Font = new Font("Segoe UI", 15F, FontStyle.Bold);
        title.ForeColor = ColorTranslator.FromHtml("#154734");   // Poly Green
        title.SetBounds(16, 12, 400, 30);
        Win.Controls.Add(title);

        Status = new Label();
        Status.Text = "Starting R...";
        Status.Font = new Font("Segoe UI", 9F);
        Status.ForeColor = ColorTranslator.FromHtml("#4a5260");
        Status.SetBounds(18, 44, 400, 18);
        Win.Controls.Add(Status);

        Label addrHdr = new Label();
        addrHdr.Text = "Point an iPad at one of these";
        addrHdr.Font = new Font("Segoe UI", 8F);
        addrHdr.ForeColor = ColorTranslator.FromHtml("#8b939f");
        addrHdr.SetBounds(18, 70, 400, 16);
        Win.Controls.Add(addrHdr);

        Addresses = new ListBox();
        Addresses.Font = new Font("Consolas", 10F);
        Addresses.SetBounds(18, 88, 394, 90);
        Addresses.BorderStyle = BorderStyle.FixedSingle;
        Win.Controls.Add(Addresses);

        OpenBtn = MakeButton("Open the app window", 18, 192, 150);
        OpenBtn.Click += delegate { OpenBrowser(); };
        CopyBtn = MakeButton("Copy address", 174, 192, 110);
        CopyBtn.Click += delegate { CopyAddress(); };
        StopBtn = MakeButton("Stop", 290, 192, 122);
        StopBtn.Click += delegate { Win.Close(); };

        // Closing the window stops the server. Anything else and a coach who
        // pressed the X at the end of a match leaves R holding the port, which
        // is exactly how 7815 ends up occupied next time.
        Win.FormClosing += delegate { StopR(); };
    }

    static Button MakeButton(string text, int x, int y, int w)
    {
        Button b = new Button();
        b.Text = text;
        b.Font = new Font("Segoe UI", 9F);
        b.SetBounds(x, y, w, 30);
        b.FlatStyle = FlatStyle.System;
        Win.Controls.Add(b);
        return b;
    }

    // ----------------------------------------------------------------- start

    static void Start(string rscript)
    {
        ProcessStartInfo si = new ProcessStartInfo(rscript, "run.R " + Port);
        si.WorkingDirectory = AppDir;
        si.UseShellExecute = false;
        si.CreateNoWindow = true;
        si.RedirectStandardOutput = true;
        si.RedirectStandardError = true;

        R = new Process();
        R.StartInfo = si;
        R.EnableRaisingEvents = true;
        R.OutputDataReceived += OnOutput;
        R.ErrorDataReceived += OnOutput;
        R.Exited += delegate { OnExit(); };

        try { File.WriteAllText(LogPath, ""); } catch { }
        R.Start();
        R.BeginOutputReadLine();
        R.BeginErrorReadLine();

        Thread t = new Thread(WaitForServer);
        t.IsBackground = true;
        t.Start();
    }

    // R's own startup text is the source of the addresses, so the window and
    // the terminal can never disagree about where this is being served.
    static readonly Regex UrlRe = new Regex(@"http://[^\s]+", RegexOptions.Compiled);

    static void OnOutput(object sender, DataReceivedEventArgs e)
    {
        if (e.Data == null) return;
        try { File.AppendAllText(LogPath, e.Data + Environment.NewLine); } catch { }
        Match m = UrlRe.Match(e.Data);
        if (!m.Success) return;
        string url = m.Value.TrimEnd('.', ',');
        if (url.Contains("0.0.0.0")) return;
        lock (Urls)
        {
            if (Urls.Contains(url)) return;
            Urls.Add(url);
        }
        OnUi(delegate { Addresses.Items.Add(url); });
    }

    static void WaitForServer()
    {
        DateTime until = DateTime.UtcNow.AddSeconds(90);
        while (DateTime.UtcNow < until)
        {
            if (R.HasExited) return;
            if (Answering(Port))
            {
                OnUi(delegate
                {
                    Status.Text = "Running. Leave this window open during the match.";
                    Status.ForeColor = ColorTranslator.FromHtml("#154734");
                });
                OpenBrowser();
                return;
            }
            Thread.Sleep(400);
        }
        OnUi(delegate { Status.Text = "R is taking longer than usual. See scoutR.log."; });
    }

    static bool Answering(int port)
    {
        try
        {
            using (TcpClient c = new TcpClient())
            {
                IAsyncResult a = c.BeginConnect(IPAddress.Loopback, port, null, null);
                if (!a.AsyncWaitHandle.WaitOne(300)) return false;
                c.EndConnect(a);
                return true;
            }
        }
        catch { return false; }
    }

    static void OnExit()
    {
        OnUi(delegate
        {
            Status.Text = "R stopped. See scoutR.log for what it said.";
            Status.ForeColor = ColorTranslator.FromHtml("#b4232a");
        });
    }

    // ------------------------------------------------------------ the buttons

    // A window with no address bar, no tabs, no bookmarks: its own taskbar
    // button and the icon of this program. Chrome and Edge both take --app
    // for exactly this.
    //
    // Why not embed a browser engine in the window itself and be done with
    // it: WebView2 needs SDK assemblies that come from NuGet, and the build
    // then stops working on a laptop with no internet, which is the one
    // place this has to be rebuildable. The control that ships inside .NET
    // is the IE11 engine, which cannot draw this app at all -- CSS grid and
    // custom properties are what the whole layout is made of. So the page is
    // rendered by a browser either way, and the only question is whether it
    // looks like one. This is the answer that costs nothing.
    //
    // --user-data-dir keeps it out of the coach's own Chrome profile, so a
    // match window is never mixed in with their tabs and closing one does
    // not close the other.
    static void OpenBrowser()
    {
        string url = "http://localhost:" + Port;
        string browser = FindBrowser();
        if (browser != null)
        {
            try
            {
                string profile = Path.Combine(
                    Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
                    "scoutR", "window");
                Directory.CreateDirectory(profile);
                ProcessStartInfo si = new ProcessStartInfo(browser,
                    "--app=" + url +
                    " --user-data-dir=\"" + profile + "\"" +
                    " --window-size=1400,900 --no-first-run --no-default-browser-check");
                si.UseShellExecute = false;
                // Held on to so Stop can close it. Safe to kill because it
                // runs in its own profile: this is never the coach's own
                // Chrome with their tabs in it.
                AppWindow = Process.Start(si);
                return;
            }
            catch { }
        }
        // No Chrome, no Edge, or it refused: a normal tab still works.
        try { Process.Start(url); } catch { }
    }

    static string FindBrowser()
    {
        string[] exes = { "chrome.exe", "msedge.exe" };
        RegistryKey[] roots = { Registry.CurrentUser, Registry.LocalMachine };
        foreach (string exe in exes)
            foreach (RegistryKey root in roots)
            {
                try
                {
                    using (RegistryKey k = root.OpenSubKey(
                        @"SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\" + exe))
                    {
                        if (k == null) continue;
                        string path = k.GetValue(null) as string;
                        if (!string.IsNullOrEmpty(path) && File.Exists(path)) return path;
                    }
                }
                catch { }
            }
        return null;
    }

    static void CopyAddress()
    {
        string url = Addresses.SelectedItem != null
            ? Addresses.SelectedItem.ToString()
            : (Urls.Count > 0 ? Urls[0] : "http://localhost:" + Port);
        try { Clipboard.SetText(url); } catch { }
        Status.Text = "Copied " + url;
    }

    // ------------------------------------------------------------------ stop

    static void StopR()
    {
        CloseAppWindow();
        if (R == null || R.HasExited) return;
        // R starts its own child for the Shiny server on some installs, so
        // killing the one we started is not enough: taskkill /T takes the
        // tree, and the port with it.
        try
        {
            ProcessStartInfo si = new ProcessStartInfo("taskkill",
                "/PID " + R.Id + " /T /F");
            si.CreateNoWindow = true;
            si.UseShellExecute = false;
            Process k = Process.Start(si);
            k.WaitForExit(4000);
        }
        catch { }
        try { if (!R.HasExited) R.Kill(); } catch { }
    }

    // Without this the match window sits there afterwards showing a page
    // that no longer loads, which looks like the app broke rather than like
    // it was closed.
    static void CloseAppWindow()
    {
        if (AppWindow == null) return;
        try
        {
            if (!AppWindow.HasExited)
            {
                ProcessStartInfo si = new ProcessStartInfo("taskkill",
                    "/PID " + AppWindow.Id + " /T /F");
                si.CreateNoWindow = true;
                si.UseShellExecute = false;
                Process k = Process.Start(si);
                k.WaitForExit(4000);
            }
        }
        catch { }
    }

    // --------------------------------------------------------------- find R

    static string FindRscript()
    {
        string[] keys = { @"SOFTWARE\R-core\R", @"SOFTWARE\WOW6432Node\R-core\R" };
        RegistryKey[] roots = { Registry.LocalMachine, Registry.CurrentUser };
        foreach (RegistryKey root in roots)
            foreach (string key in keys)
            {
                try
                {
                    using (RegistryKey k = root.OpenSubKey(key))
                    {
                        if (k == null) continue;
                        string home = k.GetValue("InstallPath") as string;
                        string exe = Candidate(home);
                        if (exe != null) return exe;
                    }
                }
                catch { }
            }

        foreach (string baseDir in new string[] {
            Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles),
            Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86),
            @"C:\" })
        {
            string rDir = Path.Combine(baseDir ?? "", "R");
            if (!Directory.Exists(rDir)) continue;
            string[] versions = Directory.GetDirectories(rDir, "R-*");
            Array.Sort(versions);
            Array.Reverse(versions);           // newest first
            foreach (string v in versions)
            {
                string exe = Candidate(v);
                if (exe != null) return exe;
            }
        }

        string path = Environment.GetEnvironmentVariable("PATH") ?? "";
        foreach (string dir in path.Split(';'))
        {
            try
            {
                string exe = Path.Combine(dir.Trim(), "Rscript.exe");
                if (File.Exists(exe)) return exe;
            }
            catch { }
        }
        return null;
    }

    static string Candidate(string home)
    {
        if (string.IsNullOrEmpty(home)) return null;
        string exe = Path.Combine(home, "bin", "Rscript.exe");
        return File.Exists(exe) ? exe : null;
    }

    // ----------------------------------------------------------------- bits

    static string AppFolder()
    {
        string dir = Path.GetDirectoryName(Application.ExecutablePath);
        // Built into build\, run from the folder above it when developing.
        if (!File.Exists(Path.Combine(dir, "run.R")))
        {
            string up = Path.GetDirectoryName(dir);
            if (up != null && File.Exists(Path.Combine(up, "run.R"))) return up;
        }
        return dir;
    }

    static int ParsePort(string s)
    {
        int p;
        return int.TryParse(s, out p) && p > 0 && p < 65536 ? p : FreePort(7815);
    }

    // The first port nothing is listening on. A previous run that did not exit
    // cleanly holds 7815, and an R stack trace about createTcpServer is not an
    // answer anybody wants at 6pm on a match day.
    static int FreePort(int from)
    {
        for (int p = from; p < from + 40; p++)
            if (!Answering(p)) return p;
        return from;
    }

    static void OnUi(MethodInvoker action)
    {
        try
        {
            if (Win != null && Win.IsHandleCreated) Win.BeginInvoke(action);
        }
        catch { }
    }
}
