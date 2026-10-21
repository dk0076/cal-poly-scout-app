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
    var el = ev.target.closest("[data-chrome],[data-pos],[data-home],[data-rail],[data-o],[data-set],[data-clr],[data-setx],[data-zone],[data-zoneclear],[data-mt],[data-tab]");
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
    // Folds the header controls away on a tablet. Local only, like the rail.
    if (el.hasAttribute("data-chrome")) {
      document.body.classList.toggle("chrome-open");
      return;
    }
    if (el.hasAttribute("data-pos")) return send("js_pos", { val: el.getAttribute("data-pos") });
    if (el.hasAttribute("data-tab")) return send("js_tab", { tab: el.getAttribute("data-tab") });
    // a panel view option: key plus the value to store
    if (el.hasAttribute("data-o"))
      return send("js_opt", { key: el.getAttribute("data-o"), val: el.getAttribute("data-v") });
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
})();
