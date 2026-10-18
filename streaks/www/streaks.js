// Star Streaks: tooltips on the stars and year bars, picking a streak or a year, and a phone flow that
// moves to the career spotlight and back.
(function () {
  var tip, picked = null, from = null;
  var smooth = window.matchMedia("(prefers-reduced-motion: reduce)").matches ? "auto" : "smooth";

  function show(el, x, y) {
    tip = tip || document.getElementById("tip");
    var p = el.dataset.tip.split("|");
    tip.innerHTML = "<strong></strong>" + p.slice(1).map(function () { return "<span></span>"; }).join("");
    tip.querySelector("strong").textContent = p[0];
    tip.querySelectorAll("span").forEach(function (s, i) { s.textContent = p[i + 1]; });
    tip.hidden = false;
    var r = tip.getBoundingClientRect();
    var left = Math.min(Math.max(8, x - r.width / 2), window.innerWidth - r.width - 8);
    var top = y - r.height - 12; if (top < 8) top = y + 18;
    tip.style.transform = "translate(" + left + "px," + top + "px)";
  }
  function hide() { if (tip) tip.hidden = true; }
  document.addEventListener("pointermove", function (e) {
    if (e.pointerType === "touch") return;
    var el = e.target.closest && e.target.closest("[data-tip]");
    if (el) show(el, e.clientX, e.clientY); else hide();
  });
  window.addEventListener("scroll", function () {
    var f = document.activeElement;
    if (!(f && f.closest && f.closest(".yr"))) hide();          // keep a keyboard-focused year's tooltip
  }, { passive: true });

  function mark() {
    document.querySelectorAll(".pq").forEach(function (p) { p.classList.toggle("is-on", p.dataset.pid === picked); });
  }

  document.addEventListener("click", function (e) {
    var head = e.target.closest(".pq__head, .now__pick");
    if (head) {
      picked = head.dataset.pid; from = head.closest(".pq") || head; mark(); hide();
      Shiny.setInputValue("pick", picked, { priority: "event" });
      if (window.innerWidth < 1000) document.getElementById("spotlight").scrollIntoView({ behavior: smooth, block: "start" });
      return;
    }
    var yr = e.target.closest("[data-year]");
    if (yr) { hide(); Shiny.setInputValue("year_pick", yr.dataset.year, { priority: "event" }); return; }
    if (e.target.closest(".more")) { Shiny.setInputValue("more", Date.now(), { priority: "event" }); return; }
    if (e.target.closest(".spot__back")) {
      (from && document.body.contains(from) ? from : document.querySelector(".walk-of-fame")).scrollIntoView({ behavior: smooth, block: "center" });
      return;
    }
    // Phones: tap a star or a year bar to read its tooltip
    var t = e.target.closest(".t[data-tip], .yr[data-tip]");
    if (t && window.matchMedia("(hover: none)").matches) { var r = t.getBoundingClientRect(); show(t, r.left + r.width / 2, r.top); }
  });

  // The year strip is one tab stop: arrow keys move between years, Enter picks one
  document.addEventListener("keydown", function (e) {
    var b = e.target.closest && e.target.closest(".yr");
    if (!b || (e.key !== "ArrowLeft" && e.key !== "ArrowRight" && e.key !== "Home" && e.key !== "End")) return;
    var all = Array.prototype.slice.call(document.querySelectorAll(".yr")), i = all.indexOf(b);
    var j = e.key === "Home" ? 0 : e.key === "End" ? all.length - 1 : Math.max(0, Math.min(all.length - 1, i + (e.key === "ArrowRight" ? 1 : -1)));
    e.preventDefault(); b.tabIndex = -1; all[j].tabIndex = 0; all[j].focus({ preventScroll: true });
    var r = all[j].getBoundingClientRect(); show(all[j], r.left + r.width / 2, r.top);
  });
  document.addEventListener("focusout", function (e) { if (e.target.closest && e.target.closest(".yr")) hide(); });

  // Phones: the strip scrolls sideways; keep a picked year in view
  $(document).on("shiny:value", function (e) {
    if (e.name !== "years") return;
    requestAnimationFrame(function () {
      var sc = document.querySelector(".yrs-scroll"), y = document.querySelector(".yr.is-on");
      if (sc && y && sc.scrollWidth > sc.clientWidth && !sc.dataset.placed) { sc.scrollLeft = y.offsetLeft - 8; sc.dataset.placed = "1"; }
    });
  });

  // A name picked in the search box becomes the spotlight too
  $(document).on("shiny:inputchanged", function (e) { if (e.name === "who" && e.value) { picked = String(e.value); mark(); } });
  $(document).on("shiny:value", function (e) { if (e.name === "walk") requestAnimationFrame(mark); });
})();
