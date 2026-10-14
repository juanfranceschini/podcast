// Scorigami: grid + timeline tooltips (hover, keyboard focus, tap), arrow keys across the grid,
// cell selection, and a phone flow that keeps the grid within reach.
(function () {
  var tip, selected = null, viaKeys = false;
  var smooth = window.matchMedia("(prefers-reduced-motion: reduce)").matches ? "auto" : "smooth";
  function show(el, x, y) {
    tip = tip || document.getElementById("tip");
    var p = el.dataset.tip.split("|");
    tip.innerHTML = "<strong>" + p[0] + "</strong>" + p.slice(1).map(function (s) { return "<span>" + s + "</span>"; }).join("");
    tip.hidden = false;
    var r = tip.getBoundingClientRect();
    var left = Math.min(Math.max(8, x - r.width / 2), window.innerWidth - r.width - 8);
    var top = y - r.height - 14; if (top < 8) top = y + 20;
    tip.style.transform = "translate(" + left + "px," + top + "px)";
  }
  // Anchor on what you see: a timeline bar's visible column, not its full-height hit area
  function showAt(el) {
    var r = (el.classList.contains("tl__bar") && el.querySelector("i") || el).getBoundingClientRect();
    show(el, r.left + r.width / 2, r.top);
  }
  function hide() { if (tip) tip.hidden = true; }
  document.addEventListener("pointermove", function (e) {
    if (e.pointerType === "touch") return;
    var el = e.target.closest && e.target.closest("[data-tip]");
    if (el) show(el, e.clientX, e.clientY); else hide();
  });
  // Keyboard focus gets the same story as hover
  document.addEventListener("keydown", function () { viaKeys = true; }, true);
  document.addEventListener("pointerdown", function () { viaKeys = false; }, true);
  document.addEventListener("focusin", function (e) {
    var el = e.target.closest && e.target.closest("[data-tip]");
    if (el && viaKeys) showAt(el);
  });
  document.addEventListener("focusout", function (e) { if (e.target.closest && e.target.closest("[data-tip]")) hide(); });
  window.addEventListener("scroll", function () {
    var f = document.activeElement;
    if (viaKeys && f && f.matches && f.matches("[data-tip]")) showAt(f); else hide();
  }, { passive: true });

  // One tab stop for the whole grid: the selected square (or the first); arrows move from there
  function mark() {
    var all = document.querySelectorAll(".g__cell"), cur = null;
    all.forEach(function (c) {
      var on = c.dataset.score === selected; c.classList.toggle("is-selected", on); c.setAttribute("aria-pressed", on);
      c.tabIndex = -1; if (on) cur = c;
    });
    if (cur || all[0]) (cur || all[0]).tabIndex = 0;
  }
  document.addEventListener("keydown", function (e) {
    var c = e.target.closest && e.target.closest(".g__cell");
    var step = { ArrowUp: [-1, 0], ArrowDown: [1, 0], ArrowLeft: [0, -1], ArrowRight: [0, 1] }[e.key];
    if (!c || !step) return;
    var p = c.dataset.score.split("-").map(Number);
    var next = document.querySelector('.g__cell[data-score="' + (p[0] + step[0]) + "-" + (p[1] + step[1]) + '"]');
    if (!next) return;
    e.preventDefault(); c.tabIndex = -1; next.tabIndex = 0; next.focus();
  });

  // Phones: after a tap, scroll only far enough to show the score and its first matches
  function revealDetail() {
    var d = document.querySelector(".card--detail"); if (!d) return;
    var top = d.getBoundingClientRect().top, room = window.innerHeight - 280;
    if (top > room) window.scrollBy({ top: top - room, behavior: smooth });
  }

  document.addEventListener("click", function (e) {
    var bar = e.target.closest(".tl__bar");
    if (bar) { showAt(bar); return; }
    var all = e.target.closest(".mt__all");
    if (all) { var list = document.querySelector(".card--detail .mt"); if (list) list.classList.add("is-open"); all.hidden = true; return; }
    if (e.target.closest(".detail__back")) {
      var target = document.querySelector(".g__cell.is-selected") || document.querySelector(".card--grid");
      target.scrollIntoView({ behavior: smooth, block: "center" }); return;
    }
    // A latest-result card lights up its square on the grid
    var card = e.target.closest(".rs[data-score]");
    if (card && !e.target.closest("a")) {
      selected = card.dataset.score; mark(); hide();
      Shiny.setInputValue("cell", selected, { priority: "event" });
      var sq = document.querySelector(".g__cell.is-selected");
      if (sq) { sq.scrollIntoView({ behavior: smooth, block: "center" }); sq.focus({ preventScroll: true }); }
      return;
    }
    var c = e.target.closest(".g__cell[data-score]");
    if (!c) { hide(); return; }
    selected = c.dataset.score; mark(); hide();
    Shiny.setInputValue("cell", selected, { priority: "event" });
    if (window.innerWidth < 900) revealDetail();
  });

  // Re-apply the selection whenever the grid is re-rendered
  document.addEventListener("DOMContentLoaded", function () {
    selected = new URLSearchParams(location.search).get("score") || null;
    new MutationObserver(function () { if (!selected) { var s = new URLSearchParams(location.search).get("score"); if (s) selected = s; } mark(); })
      .observe(document.getElementById("grid"), { childList: true });
  });
})();
