/* ---------------------------------------------------------------------------
   One delegated click handler for every control the server renders as raw
   HTML. The alternative -- a Shiny input widget per chip -- would mean the
   server growing an observer for each of the ~80 controls across the tabs, and
   a full UI rebuild on every click. Here the panel HTML stays plain strings
   and each control just names the state key it sets.

   Every message carries a nonce, because Shiny collapses a repeated identical
   input value: clicking the same chip twice (on, then off) must register as
   two events.
   ------------------------------------------------------------------------- */
(function () {
  var n = 0;
  function send(name, payload) {
    payload.nonce = ++n;
    Shiny.setInputValue(name, payload, { priority: "event" });
  }

  document.addEventListener("click", function (ev) {
    var el = ev.target.closest("[data-detail],[data-simplify],[data-tabpick],[data-chrome],[data-pos],[data-home],[data-rail],[data-o],[data-set],[data-clr],[data-setx],[data-zone],[data-zoneclear],[data-mt],[data-tab]");
    if (!el) return;

    // Folds the filter rail away on a tablet. Purely local: nothing the
    // server needs to know, and a round trip would make it feel slow.
    // Start over. Confirmed, because the watch is shared: one stray tap on a
    // tablet would otherwise take the whole bench off the live match.
    if (el.hasAttribute("data-home")) {
      if (document.getElementById("intro")) return;
      if (window.confirm("Start over? This clears the loaded files and stops following the live file, for everyone watching."))
        send("js_home", {});
      return;
    }
    if (el.hasAttribute("data-rail")) {
      document.body.classList.toggle("rail-open");
      return;
    }
    if (el.hasAttribute("data-detail")) {
      toggleDetail();
      return;
    }
    if (el.hasAttribute("data-simplify")) {
      toggleSimplify();
      return;
    }
    // Which tabs this device shows. Local only: one laptop serves every
    // iPad, so this must not reach the server.
    if (el.hasAttribute("data-tabpick")) {
      tabPanel();
      return;
    }
    // Folds the header controls away on a tablet. Local only, like the rail.
    if (el.hasAttribute("data-chrome")) {
      document.body.classList.toggle("chrome-open");
      return;
    }
    if (el.hasAttribute("data-pos")) return send("js_pos", { val: el.getAttribute("data-pos") });
    if (el.hasAttribute("data-tab")) return send("js_tab", { tab: el.getAttribute("data-tab") });
    // a panel view option: key plus the value to store
    if (el.hasAttribute("data-o")) {
      var key = el.getAttribute("data-o"), val = el.getAttribute("data-v");
      if (key === "charts") rememberCharts(val);
      return send("js_opt", { key: key, val: val });
    }
    // a rail multi-select: toggle one value in that set
    if (el.hasAttribute("data-set"))
      return send("js_sel", { key: el.getAttribute("data-set"), val: el.getAttribute("data-v") });
    if (el.hasAttribute("data-clr")) return send("js_clear", { key: el.getAttribute("data-clr") });
    if (el.hasAttribute("data-setx")) return send("js_setx", { val: el.getAttribute("data-setx") });
    if (el.hasAttribute("data-zone")) return send("js_zone", { val: el.getAttribute("data-zone") });
    if (el.hasAttribute("data-zoneclear")) return send("js_zoneclear", {});
    if (el.hasAttribute("data-mt")) return send("js_match", { val: el.getAttribute("data-mt") });
  });

  // Per-file side assignment. Rendered as a <select> inside the rail HTML, so
  // it needs the same delegated treatment as the chips.
  document.addEventListener("change", function (ev) {
    var el = ev.target;
    if (el.matches && el.matches("select[data-focus]"))
      send("js_side", { id: el.getAttribute("data-focus"), val: el.value });
  });

  /* ---- drop zone -------------------------------------------------------
     The zone is a plain div, not a second file input: Shiny's uploader is
     bound to the one in the header, and two inputs sharing an id send the
     upload to whichever bound last. So a drop hands its files to that input
     and fires the change event the binding already listens for. */
  function fileInput() { return document.getElementById("files"); }

  function handOver(files) {
    var inp = fileInput();
    if (!inp || !files || !files.length) return;
    var dt = new DataTransfer();
    for (var i = 0; i < files.length; i++) dt.items.add(files[i]);
    inp.files = dt.files;
    inp.dispatchEvent(new Event("change", { bubbles: true }));
  }

  function zone(ev) {
    return ev.target && ev.target.closest ? ev.target.closest("#drop") : null;
  }

  // Without these two the browser just navigates to the dropped file.
  ["dragenter", "dragover"].forEach(function (name) {
    document.addEventListener(name, function (ev) {
      var z = zone(ev);
      if (!z) return;
      ev.preventDefault();
      ev.dataTransfer.dropEffect = "copy";
      z.classList.add("hot");
    });
  });
  ["dragleave", "dragend"].forEach(function (name) {
    document.addEventListener(name, function (ev) {
      var z = zone(ev);
      if (z) z.classList.remove("hot");
    });
  });
  document.addEventListener("drop", function (ev) {
    var z = zone(ev);
    if (!z) return;
    ev.preventDefault();
    z.classList.remove("hot");
    handOver(ev.dataTransfer.files);
  });

  // click or keyboard opens the same picker the header button would
  document.addEventListener("click", function (ev) {
    if (zone(ev)) { var i = fileInput(); if (i) i.click(); }
  });
  document.addEventListener("keydown", function (ev) {
    if ((ev.key === "Enter" || ev.key === " ") && zone(ev)) {
      ev.preventDefault();
      var i = fileInput(); if (i) i.click();
    }
  });

  /* ---- which tabs this device shows ------------------------------------
     One laptop serves every iPad on the bench, so this cannot live on the
     server: the coach watching serve and pass and the coach watching attack
     and defence are looking at the same session. It is a property of the
     device, so the device remembers it, in localStorage, which also means it
     survives a reload and needs no round trip to apply.

     Hidden tabs are stored rather than shown ones. A tab added in a later
     version then appears by default instead of being invisible to everyone
     who had already set a preference. */
  var TABS_KEY = "scoutr.hiddenTabs";
  var CHARTS_KEY = "scoutr.charts";

  // The chart switch lives on the server, like the other view options, so a
  // reload would hand the courts back to a coach who had just taken them
  // off. The choice is this device's, like the tab picker, so this browser
  // remembers it and says so again as soon as it reconnects.
  function rememberCharts(val) {
    try { window.localStorage.setItem(CHARTS_KEY, val); }
    catch (e) { /* the choice lasts this page only */ }
  }

  // How far down the sticky tab bar has to sit. The header wraps to two rows
  // at some widths, and a fixed offset would either leave a gap under it or
  // cut the first row of numbers in half. Measured, not guessed.
  function measureHeader() {
    var h = document.querySelector("header.appbar");
    if (!h) return;
    var stuck = getComputedStyle(h).position === "sticky";
    document.documentElement.style.setProperty(
      "--appbar-h", (stuck ? Math.round(h.getBoundingClientRect().height) : 0) + "px");
  }
  window.addEventListener("resize", measureHeader);
  window.addEventListener("load", measureHeader);

  var charts_said = false;
  function restoreCharts() {
    if (charts_said) return;
    var v = null;
    try { v = window.localStorage.getItem(CHARTS_KEY); } catch (e) { v = null; }
    charts_said = true;
    // Only the off case: on is the server's own default, and sending it
    // would rebuild the page for nothing.
    if (v === "false") send("js_opt", { key: "charts", val: "false" });
  }

  // Through jQuery, not addEventListener. Shiny raises shiny:connected with
  // jQuery's own trigger(), which never becomes a real DOM event, so a
  // native listener sits there and never hears it. That is why this looked
  // like it worked and the courts came back on every reload.
  if (window.jQuery) window.jQuery(document).on("shiny:connected", restoreCharts);
  // And if the socket is already up by the time this file runs, the event
  // has been and gone.
  if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected &&
      Shiny.shinyapp.isConnected()) restoreCharts();
  var DETAIL_KEY = "scoutr.showDetail";

  function detailOn() {
    try { return window.localStorage.getItem(DETAIL_KEY) === "1"; }
    catch (e) { return false; }
  }

  function applyDetail() {
    var on = detailOn();
    document.body.classList.toggle("detail-open", on);
    var b = document.querySelector("[data-detail]");
    if (b) b.textContent = on ? "Less detail" : "Detail";
  }

  function toggleDetail() {
    try { window.localStorage.setItem(DETAIL_KEY, detailOn() ? "0" : "1"); }
    catch (e) { /* the choice lasts this page only */ }
    applyDetail();
  }

  /* The three a coach asked for: the attacking numbers, the passing numbers,
     and the rotational analysis. Everything else is one tap away in Tabs.

     Deliberately not a tab per question. Three is a bar you read without
     looking for anything, which is the point of pressing Simplify at all. */
  var MATCH_DAY = ["attack", "passing", "rotation"];

  function simplified() {
    var hidden = hiddenTabs();
    var all = [].slice.call(document.querySelectorAll(".tabs .tab"))
      .map(function (b) { return b.getAttribute("data-tab"); });
    if (!all.length) return false;
    return all.every(function (id) {
      return MATCH_DAY.indexOf(id) !== -1 ? hidden.indexOf(id) === -1
                                          : hidden.indexOf(id) !== -1;
    });
  }

  function toggleSimplify() {
    // Pressed before the tab bar exists there is nothing to simplify, and
    // writing an empty list would quietly wipe a choice already saved.
    if (!document.querySelectorAll(".tabs .tab").length) return;
    if (simplified()) {
      setHiddenTabs([]);
    } else {
      var all = [].slice.call(document.querySelectorAll(".tabs .tab"))
        .map(function (b) { return b.getAttribute("data-tab"); });
      setHiddenTabs(all.filter(function (id) { return MATCH_DAY.indexOf(id) === -1; }));
    }
    applyTabs();
    var p = document.getElementById("tabpick");
    if (p) p.remove();
  }

  // The button says what pressing it will do, and it is the client that
  // knows, since the choice lives in this browser and never reaches R.
  function labelSimplify() {
    var b = document.querySelector("[data-simplify]");
    if (b) b.textContent = simplified() ? "All tabs" : "Simplify";
  }

  function hiddenTabs() {
    try {
      var raw = window.localStorage.getItem(TABS_KEY);
      return raw ? JSON.parse(raw) : [];
    } catch (e) {
      // Private browsing, or storage switched off. Show everything rather
      // than fail: a coach with no tabs is worse off than one with all.
      return [];
    }
  }

  function setHiddenTabs(list) {
    try {
      window.localStorage.setItem(TABS_KEY, JSON.stringify(list));
    } catch (e) { /* nothing to do; the choice lasts this page only */ }
  }

  function applyTabs() {
    var hidden = hiddenTabs();
    var all = [].slice.call(document.querySelectorAll(".tabs .tab"));
    if (!all.length) return;
    var shown = [];
    all.forEach(function (b) {
      var id = b.getAttribute("data-tab");
      var off = hidden.indexOf(id) !== -1;
      b.style.display = off ? "none" : "";
      if (!off) shown.push(b);
    });
    // Never leave a coach with nothing. If every tab is off, the setting is
    // ignored rather than obeyed.
    if (!shown.length) {
      all.forEach(function (b) { b.style.display = ""; });
      return;
    }
    // Hiding the tab you are on would leave the panel below showing a tab
    // that is no longer in the bar, so move to the first one still there.
    var active = document.querySelector(".tabs .tab.on");
    if (active && active.style.display === "none") shown[0].click();
    labelSimplify();
    applyDetail();
    measureHeader();
  }

  function tabPanel() {
    var old = document.getElementById("tabpick");
    if (old) { old.remove(); return; }
    var hidden = hiddenTabs();
    var wrap = document.createElement("div");
    wrap.id = "tabpick";
    var h = document.createElement("div");
    h.className = "tabpick-h";
    h.textContent = "Tabs on this device";
    wrap.appendChild(h);
    [].slice.call(document.querySelectorAll(".tabs .tab")).forEach(function (b) {
      var id = b.getAttribute("data-tab");
      var on = hidden.indexOf(id) === -1;
      var row = document.createElement("button");
      row.className = "chip wide" + (on ? " on" : "");
      row.textContent = b.textContent.trim();
      row.addEventListener("click", function (ev) {
        ev.stopPropagation();
        var list = hiddenTabs();
        var i = list.indexOf(id);
        if (i === -1) list.push(id); else list.splice(i, 1);
        setHiddenTabs(list);
        row.classList.toggle("on");
        applyTabs();
      });
      wrap.appendChild(row);
    });
    var simp = document.createElement("button");
    simp.className = "chip ghost";
    simp.textContent = simplified() ? "All tabs" : "Simplify to match day";
    simp.addEventListener("click", function (ev) {
      ev.stopPropagation();
      toggleSimplify();
    });
    wrap.appendChild(simp);

    /* With everything already on, "Show all" does nothing, so it becomes
       the opposite: clear the lot and keep the tab being read, since a bar
       with nothing in it is not a state worth being able to reach. */
    var allOn = hidden.length === 0;
    var foot = document.createElement("button");
    foot.className = "chip ghost";
    foot.textContent = allOn ? "Clear all" : "Show all";
    foot.addEventListener("click", function (ev) {
      ev.stopPropagation();
      if (!allOn) {
        setHiddenTabs([]);
      } else {
        var all = [].slice.call(document.querySelectorAll(".tabs .tab"))
          .map(function (b) { return b.getAttribute("data-tab"); });
        if (!all.length) return;
        var active = document.querySelector(".tabs .tab.on");
        var keep = active ? active.getAttribute("data-tab") : all[0];
        setHiddenTabs(all.filter(function (id) { return id !== keep; }));
      }
      applyTabs();
      wrap.remove();
    });
    wrap.appendChild(foot);
    document.body.appendChild(wrap);
    var btn = document.querySelector("[data-tabpick]");
    if (btn) {
      var r = btn.getBoundingClientRect();
      wrap.style.top = Math.round(r.bottom + window.scrollY + 6) + "px";
      wrap.style.left = Math.round(Math.min(r.left, window.innerWidth - 230)) + "px";
    }
  }

  /* The tab bar does not exist when this script runs: it is inside an output
     the server renders once the session connects, and it is replaced again on
     every panel change, which throws away the inline styles. Watching the
     document rather than the bar is the version that survives both, since
     there is nothing to attach to at load time. */
  // setTimeout rather than requestAnimationFrame: a page in a background
  // tab gets no animation frames, so an iPad left on another app would come
  // back showing every tab again.
  var pending = false;
  function scheduleApply() {
    if (pending) return;
    pending = true;
    setTimeout(function () { pending = false; applyTabs(); }, 0);
  }
  new MutationObserver(scheduleApply).observe(document.documentElement,
    { childList: true, subtree: true });
  document.addEventListener("shiny:value", scheduleApply);
  if (document.readyState === "loading")
    document.addEventListener("DOMContentLoaded", scheduleApply);
  else scheduleApply();

  document.addEventListener("click", function (ev) {
    var p = document.getElementById("tabpick");
    if (p && !p.contains(ev.target) && !ev.target.closest("[data-tabpick]")) p.remove();
  });

})();
