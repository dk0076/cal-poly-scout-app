# ---------------------------------------------------------------------------
#  Build scoutR.exe.
#
#      powershell -ExecutionPolicy Bypass -File build\make-exe.ps1
#
#  Uses the C# compiler that ships inside Windows (csc.exe, part of .NET
#  Framework), so there is nothing to install and this works on a laptop in a
#  gym with no internet. Takes about a second.
#
#  The icon is made here too, from the Mustangs mark the app already serves,
#  rather than kept as a separate binary in the repository that can drift
#  from it. Windows accepts a PNG inside an .ico, so this is a header and the
#  file's own bytes, no image library involved.
# ---------------------------------------------------------------------------

$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
$build = Join-Path $root 'build'
$png = Join-Path $root 'www\cp-mark@2x.png'
$ico = Join-Path $build 'scoutR.ico'
$src = Join-Path $build 'scoutR.cs'
$out = Join-Path $root 'scoutR.exe'

# ---- the icon --------------------------------------------------------------

function New-IcoFromPng {
    param([string]$Png, [string]$Ico)

    $bytes = [System.IO.File]::ReadAllBytes($Png)
    # PNG header: 8 byte signature, then the IHDR chunk, whose first two
    # fields are width and height as big-endian 32-bit integers.
    $w = [BitConverter]::ToUInt32($bytes[19..16], 0)
    $h = [BitConverter]::ToUInt32($bytes[23..20], 0)
    if ($w -gt 256 -or $h -gt 256) {
        throw "cp-mark@2x.png is ${w}x${h}; an icon has to be 256 or smaller."
    }

    $ms = New-Object System.IO.MemoryStream
    $bw = New-Object System.IO.BinaryWriter($ms)
    $bw.Write([UInt16]0)        # reserved
    $bw.Write([UInt16]1)        # type: icon
    $bw.Write([UInt16]1)        # one image
    # 256 is written as 0 in a directory entry, which is why a 256px icon
    # looks like a zero-sized one in a hex dump.
    $bw.Write([Byte]($(if ($w -eq 256) { 0 } else { $w })))
    $bw.Write([Byte]($(if ($h -eq 256) { 0 } else { $h })))
    $bw.Write([Byte]0)          # palette entries
    $bw.Write([Byte]0)          # reserved
    $bw.Write([UInt16]1)        # colour planes
    $bw.Write([UInt16]32)       # bits per pixel
    $bw.Write([UInt32]$bytes.Length)
    $bw.Write([UInt32]22)       # offset: 6 byte header + 16 byte entry
    $bw.Write($bytes)
    $bw.Flush()
    [System.IO.File]::WriteAllBytes($Ico, $ms.ToArray())
    $bw.Dispose(); $ms.Dispose()
    Write-Host ("  icon    {0}x{1} from {2}" -f $w, $h, (Split-Path -Leaf $Png))
}

# ---- the compiler ----------------------------------------------------------

$csc = Get-ChildItem "$env:WINDIR\Microsoft.NET\Framework64" -Filter csc.exe -Recurse -ErrorAction SilentlyContinue |
       Sort-Object FullName | Select-Object -Last 1
if (-not $csc) {
    $csc = Get-ChildItem "$env:WINDIR\Microsoft.NET\Framework" -Filter csc.exe -Recurse -ErrorAction SilentlyContinue |
           Sort-Object FullName | Select-Object -Last 1
}
if (-not $csc) {
    throw "No C# compiler found under $env:WINDIR\Microsoft.NET. Install .NET Framework 4, or keep using scout.bat."
}

Write-Host ""
Write-Host "  Building scoutR.exe"
New-IcoFromPng -Png $png -Ico $ico
Write-Host ("  compiler {0}" -f $csc.FullName)

$refs = @('System.dll', 'System.Drawing.dll', 'System.Windows.Forms.dll')
$args = @(
    '/nologo', '/target:winexe', '/optimize+',
    "/out:$out", "/win32icon:$ico"
) + ($refs | ForEach-Object { "/r:$_" }) + @($src)

& $csc.FullName @args
if ($LASTEXITCODE -ne 0) { throw "csc failed with exit code $LASTEXITCODE" }

$size = [math]::Round((Get-Item $out).Length / 1KB, 1)
Write-Host ("  built   {0} ({1} KB)" -f $out, $size)
Write-Host ""
Write-Host "  Double-click scoutR.exe to run a match. R still does the work:"
Write-Host "  this starts it, picks a free port, waits for it to answer and"
Write-Host "  opens the browser. Closing its window stops the server."
Write-Host ""
