// Scorigami: grid + timeline tooltips, cell selection.
(function () {
  var tip, selected = null;
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
  function hide() { if (tip) tip.hidden = true; }
  document.addEventListener("pointermove", function (e) {
    var el = e.target.closest && e.target.closest("[data-tip]");
    if (el && e.pointerType !== "touch") show(el, e.clientX, e.clientY); else hide();
  });
  window.addEventListener("scroll", hide, { passive: true });

  function mark() {
    document.querySelectorAll(".g__cell").forEach(function (c) {
      var on = c.dataset.score === selected; c.classList.toggle("is-selected", on); c.setAttribute("aria-pressed", on);
    });
  }
  document.addEventListener("click", function (e) {
    var c = e.target.closest(".g__cell[data-score]");
    if (!c) return;
    selected = c.dataset.score; mark(); hide();
    Shiny.setInputValue("cell", selected, { priority: "event" });
    if (window.innerWidth < 900) document.querySelector(".card--detail").scrollIntoView({ behavior: "smooth", block: "start" });
  });
  // Re-apply the selection whenever the grid is re-rendered
  document.addEventListener("DOMContentLoaded", function () {
    selected = new URLSearchParams(location.search).get("score") || null;
    new MutationObserver(function () { if (!selected) { var s = new URLSearchParams(location.search).get("score"); if (s) selected = s; } mark(); })
      .observe(document.getElementById("grid"), { childList: true });
  });
})();
