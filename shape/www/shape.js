// The Shape of Sport: the grid. Every athlete on graph paper between two tape measures. Hovering reads the tape,
// a click pins, pinch or wheel zooms, a drag pans. R decides who is in view and writes the cards (app.R).
(function () {
  "use strict";
  var BASE = { x0: 45, x1: 178, y0: 155, y1: 232 }, MAXZ = 12;      // kg across, cm up: every athlete plus a margin; up to 12x
  var KG_LB = 0.45359237, CM_IN = 2.54;
  var INK = "#1d2622", PAPER = "#f5f1e6", TAPE = "#f2c230", TAPE_LO = "#d9a91c", TAPE_RED = "#b8291f";
  // Spread: each dot is drawn up to ±0.45 in and ±0.45 lb from its listed size (fixed per player, from prep.R), so it
  // never leaves its own whole-inch, whole-pound listing. Readings, cards and "same size" always use the listed value.
  var SPREAD_H = 0.45 * CM_IN / 100, SPREAD_W = 0.45 * KG_LB / 100;
  var D, N = 0, sportOf, color, fill, fillDim, drawOrder, contours = null, chipsOn = null;
  var vis, visList = [], groups = new Map(), index = { size: 18, b: new Map() };
  var S = { units: "metric", view: copy(BASE), spread: true, contours: true, narrowed: false, meAt: 0, pinned: -1, hovered: -1, me: null, matches: [], q: false, total: 0, active: 0, open: false };
  var canvas, ctx, overlay, octx, board, tip, msg, axY, axX, W = 1, H = 1, dpr = 1, TW = 46, TH = 30, box = { left: 46, top: 10, width: 1, height: 1 };
  var drag = null, frame = 0, ready = false, queue = [], hintTimer;
  var nf = new Intl.NumberFormat("en-US");
  var finePointer = window.matchMedia("(hover: hover) and (pointer: fine)");
  var reduceMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
  var NARROW = 980;                                   // below this the card sits under the grid
  var PENCIL = "#3b423e", HAND = "600 19px Caveat, 'Segoe Print', cursive";

  function $(id) { return document.getElementById(id); }
  function copy(v) { return { x0: v.x0, x1: v.x1, y0: v.y0, y1: v.y1 }; }
  function arr(x) { return x == null ? [] : [].concat(x); }
  function esc(s) { return String(s).replace(/[&<>"']/g, function (c) { return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]; }); }
  function event(name, value) { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue(name, value, { priority: "event" }); }
  function f1(x) { var r = Math.round(x * 10) / 10; return Math.abs(r % 1) < 1e-9 ? r.toFixed(0) : r.toFixed(1); }
  function ftin(inches) { var ft = Math.floor(inches / 12), i = Math.round((inches - ft * 12) * 10) / 10; if (i >= 12) { ft++; i = 0; } return ft + "′ " + f1(i) + "″"; }
  function sizeOf(h, w, hin, lb) {
    return S.units === "metric" ? { h: f1(h), hu: "cm", w: f1(w), wu: "kg" } : { h: ftin(hin), hu: "", w: f1(lb), wu: "lb" };
  }
  function sizeOfPlayer(j) { return sizeOf(D.h[j], D.w[j], D.hin[j], D.lb[j]); }
  function key(j) { return D.h[j] + ":" + D.w[j]; }
  function dh(j) { return S.spread ? D.h[j] + D.jh[j] * SPREAD_H : D.h[j]; }      // where a dot is drawn
  function dw(j) { return S.spread ? D.w[j] + D.jw[j] * SPREAD_W : D.w[j]; }
  function dot(c) { return '<i class="dot" style="--c:' + c + '"></i>'; }

  // ---- geometry: metric values are canonical; units only change the tape -----------------------------
  function px(kg) { return box.left + (kg - S.view.x0) / (S.view.x1 - S.view.x0) * box.width; }
  function py(cm) { return box.top + (S.view.y1 - cm) / (S.view.y1 - S.view.y0) * box.height; }
  function unx(x) { return S.view.x0 + (x - box.left) / box.width * (S.view.x1 - S.view.x0); }
  function uny(y) { return S.view.y1 - (y - box.top) / box.height * (S.view.y1 - S.view.y0); }
  function inside(x, y) { return x >= box.left && x <= box.left + box.width && y >= box.top && y <= box.top + box.height; }
  function zoom() { return (BASE.x1 - BASE.x0) / (S.view.x1 - S.view.x0); }
  function clampView(v) {
    var bw = BASE.x1 - BASE.x0, bh = BASE.y1 - BASE.y0;
    var w = Math.min(bw, Math.max(bw / MAXZ, v.x1 - v.x0)), h = Math.min(bh, Math.max(bh / MAXZ, v.y1 - v.y0));
    var x0 = Math.min(BASE.x1 - w, Math.max(BASE.x0, v.x0)), y0 = Math.min(BASE.y1 - h, Math.max(BASE.y0, v.y0));
    return { x0: x0, x1: x0 + w, y0: y0, y1: y0 + h };
  }
  function zoomAt(factor, x, y) {
    var v = S.view, w = v.x1 - v.x0, h = v.y1 - v.y0, bw = BASE.x1 - BASE.x0;
    if (x == null) { x = box.left + box.width / 2; y = box.top + box.height / 2; }
    var nw = Math.max(bw / MAXZ, Math.min(bw, w / factor)), nh = h * nw / w, ax = unx(x), ay = uny(y);
    var x0 = ax - (ax - v.x0) / w * nw, y0 = ay - (ay - v.y0) / h * nh;
    S.view = clampView({ x0: x0, x1: x0 + nw, y0: y0, y1: y0 + nh });
    S.hovered = -1; hideTip(); schedule();
  }
  function panFrom(v, dx, dy) {
    var sx = dx / box.width * (v.x1 - v.x0), sy = dy / box.height * (v.y1 - v.y0);
    return clampView({ x0: v.x0 - sx, x1: v.x1 - sx, y0: v.y0 + sy, y1: v.y1 + sy });
  }

  // ---- tape steps: minor ticks at least 5px apart, numbers far enough apart to read -----------------
  var STEPS = {
    m:  { minor: [0.5, 1, 2, 5, 10], major: [1, 2, 5, 10, 20, 50] },
    in: { minor: [0.5, 1, 2, 3, 6], major: [1, 2, 3, 6, 12] },
    lb: { minor: [1, 2, 5, 10, 20], major: [5, 10, 20, 25, 50, 100] }
  };
  function steps(kind, span, len, gap) {
    var s = STEPS[kind], per = len / span, minor = s.minor[s.minor.length - 1], major = s.major[s.major.length - 1], k;
    for (k = 0; k < s.minor.length; k++) if (s.minor[k] * per >= 5) { minor = s.minor[k]; break; }
    for (k = 0; k < s.major.length; k++) {
      var m = s.major[k];
      if (m >= minor && Math.abs(m / minor - Math.round(m / minor)) < 1e-9 && m * per >= gap) { major = m; break; }
    }
    return { minor: minor, major: major, per: per };
  }
  function each(lo, hi, step, fn) {
    for (var k = Math.ceil(lo / step - 1e-9); k <= Math.floor(hi / step + 1e-9); k++) fn(Math.round(k * step * 1000) / 1000);
  }
  function isMult(v, m) { return Math.abs(v / m - Math.round(v / m)) < 1e-6; }

  // ---- drawing ------------------------------------------------------------------------------------------
  function resize() {
    var r = board.getBoundingClientRect(); W = r.width; H = r.height;
    if (W < 1 || H < 1) return;
    dpr = Math.min(window.devicePixelRatio || 1, 2.5);
    [canvas, overlay].forEach(function (c) { c.width = Math.round(W * dpr); c.height = Math.round(H * dpr); });
    TW = W < 560 ? 40 : 46; TH = 30;
    box = { left: TW, top: 10, width: Math.max(1, W - TW - 12), height: Math.max(1, H - TH - 10) };
    axY.style.left = (TW + 10) + "px"; axX.style.bottom = (TH + 8) + "px";
    S.hovered = -1; hideTip(); schedule();
  }
  function schedule() { if (!frame) frame = requestAnimationFrame(function () { frame = 0; draw(); }); }
  function hline(c, x0, x1, y, col, lw) { c.strokeStyle = col; c.lineWidth = lw; c.beginPath(); c.moveTo(x0, Math.round(y) + 0.5); c.lineTo(x1, Math.round(y) + 0.5); c.stroke(); }
  function vline(c, x, y0, y1, col, lw) { c.strokeStyle = col; c.lineWidth = lw; c.beginPath(); c.moveTo(Math.round(x) + 0.5, y0); c.lineTo(Math.round(x) + 0.5, y1); c.stroke(); }

  function draw() {
    if (!ready) return;
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0); ctx.clearRect(0, 0, W, H);
    var metric = S.units === "metric", xu = metric ? 1 : KG_LB, yu = metric ? 1 : CM_IN, v = S.view;
    var X = { lo: v.x0 / xu, hi: v.x1 / xu, at: function (u) { return px(u * xu); } };
    var Y = { lo: v.y0 / yu, hi: v.y1 / yu, at: function (u) { return py(u * yu); } };
    var tx = steps(metric ? "m" : "lb", X.hi - X.lo, box.width, W < 560 ? 38 : 56), ty = steps(metric ? "m" : "in", Y.hi - Y.lo, box.height, 38);

    // graph paper
    ctx.save(); ctx.beginPath(); ctx.rect(box.left, box.top, box.width, box.height); ctx.clip();
    each(X.lo, X.hi, tx.minor, function (u) { vline(ctx, X.at(u), box.top, box.top + box.height, isMult(u, tx.major) ? "rgba(52,104,80,.22)" : "rgba(52,104,80,.08)", 1); });
    each(Y.lo, Y.hi, ty.minor, function (u) { hline(ctx, box.left, box.left + box.width, Y.at(u), isMult(u, ty.major) ? "rgba(52,104,80,.22)" : "rgba(52,104,80,.08)", 1); });

    // athletes, in a fixed shuffled order so no sport always sits on top
    // every dot gets a thin ink outline, so overlapping players read as separate marks
    var z = zoom(), r = (W < 560 ? 2.4 : 2.9) + Math.log2(z) * 0.55;
    var lines = S.contours && contours, fills = lines ? fillDim : fill;      // with contours on, the dots step back
    var e = S.me ? reveal() : 1;
    ctx.globalAlpha = e < 1 ? dimAt(e) : 1;
    ctx.strokeStyle = lines ? "rgba(29,38,34,.34)" : "rgba(29,38,34,.78)"; ctx.lineWidth = z > 2.5 ? 0.9 : 0.7;
    for (var k = 0; k < N; k++) {
      var j = drawOrder[k], x = dw(j), y = dh(j);
      if (!vis[j] || x < v.x0 || x > v.x1 || y < v.y0 || y > v.y1) continue;
      ctx.beginPath(); ctx.arc(px(x), py(y), r, 0, 6.2832); ctx.fillStyle = fills[j]; ctx.fill(); ctx.stroke();
    }
    ctx.globalAlpha = 1;
    if (lines) drawContours();
    if (lines && notesOn()) drawNotes();
    if (S.me) drawMe(e);
    ctx.restore();

    drawTapes(X, Y, tx, ty, metric);
    if (S.me) drawMeTicks(e);
    buildIndex();
    $("zoom").textContent = z.toFixed(1) + "×";
    $("zoom-in").disabled = z >= MAXZ - 0.01; $("zoom-out").disabled = z <= 1.001; $("zoom-reset").disabled = z <= 1.001;
    board.classList.toggle("is-zoomed", z > 1.001);
    drawOverlay();
    if (e < 1) schedule();                               // keep the reveal running
  }

  // ---- "you": a doorframe mark. A bold pencil tick across each tape, faint guides to an X at your size, and a
  // handwritten "you". The first time it appears the cloud dims for a beat while the guides are drawn.
  function reveal() { return S.meAt ? Math.min(1, (performance.now() - S.meAt) / 1500) : 1; }
  function ease(t) { return 1 - Math.pow(1 - t, 3); }
  function dimAt(e) { return e < .25 ? 1 - .65 * ease(e / .25) : e < .6 ? .35 : .35 + .65 * ease((e - .6) / .4); }
  function drawMe(e) {
    var x = px(S.me.w), y = py(S.me.h), g = Math.max(0, Math.min(1, (e - .1) / .45));
    if (!inside(x, y) || g <= 0) return;
    ctx.save(); ctx.strokeStyle = "rgba(59,66,62,.45)"; ctx.lineWidth = 1; ctx.lineCap = "round";
    ctx.beginPath(); ctx.moveTo(box.left, y); ctx.lineTo(box.left + (x - box.left) * g, y);
    ctx.moveTo(x, box.top + box.height); ctx.lineTo(x, box.top + box.height - (box.top + box.height - y) * g); ctx.stroke();
    if (g >= 1) {
      ctx.strokeStyle = PENCIL; ctx.lineWidth = 2.4; ctx.beginPath();
      ctx.moveTo(x - 6, y - 6); ctx.lineTo(x + 6, y + 6); ctx.moveTo(x + 6, y - 6); ctx.lineTo(x - 6, y + 6); ctx.stroke();
      ctx.font = "600 24px Caveat, 'Segoe Print', cursive"; ctx.textBaseline = "alphabetic";
      var tw = ctx.measureText("you").width, lx = x + 10 + tw > box.left + box.width ? x - 10 - tw : x + 10, ly = y - 10 < box.top + 20 ? y + 26 : y - 10;
      ctx.strokeStyle = "rgba(245,241,230,.9)"; ctx.lineWidth = 5; ctx.lineJoin = "round"; ctx.strokeText("you", lx, ly);
      ctx.fillStyle = PENCIL; ctx.fillText("you", lx, ly);
    }
    ctx.restore();
  }
  function drawMeTicks(e) {
    var x = px(S.me.w), y = py(S.me.h), bottom = H - TH;
    if (e < .1) return;
    ctx.save(); ctx.strokeStyle = PENCIL; ctx.lineWidth = 2.6; ctx.lineCap = "round"; ctx.beginPath();
    if (y >= box.top && y <= box.top + box.height) { ctx.moveTo(2, y); ctx.lineTo(TW + 12, y); }
    if (x >= box.left && x <= box.left + box.width) { ctx.moveTo(x, H - 2); ctx.lineTo(x, bottom - 12); }
    ctx.stroke(); ctx.restore();
  }

  // ---- three pencil notes on the opening view (all leagues, no zoom): what the contours show, in numbers from R
  function notesOn() { return S.total === N && zoom() < 1.4 && W >= 640 && arr(D.notes).length > 0; }
  function drawNotes() {
    ctx.save(); ctx.font = HAND; ctx.textBaseline = "alphabetic"; ctx.textAlign = "left"; ctx.lineCap = "round"; ctx.lineJoin = "round";
    arr(D.notes).forEach(function (n) {
      var lines = arr(S.units === "metric" ? n.metric : n.imperial), lh = 20, x = px(n.lw), y = py(n.lh);
      var wmax = Math.max.apply(null, lines.map(function (t) { return ctx.measureText(t).width; }));
      x = Math.max(box.left + 8, Math.min(box.left + box.width - 8 - wmax, x));
      var ax = px(n.aw), ay = py(n.ah), below = ay > y, sy = below ? y + (lines.length - 1) * lh + 7 : y - lh + 2;
      var sx = Math.max(x + 6, Math.min(x + wmax - 6, ax));
      ctx.strokeStyle = "rgba(59,66,62,.7)"; ctx.lineWidth = 1.2; ctx.beginPath(); ctx.moveTo(sx, sy);
      ctx.quadraticCurveTo(sx + (ax - sx) * .2 + 14, sy + (ay - sy) * .6, ax, ay); ctx.stroke();
      ctx.fillStyle = PENCIL; ctx.beginPath(); ctx.arc(ax, ay, 2.4, 0, 6.2832); ctx.fill();
      lines.forEach(function (t, k) {
        ctx.strokeStyle = "rgba(245,241,230,.92)"; ctx.lineWidth = 5; ctx.strokeText(t, x, y + k * lh);
        ctx.fillStyle = PENCIL; ctx.fillText(t, x, y + k * lh);
      });
    });
    ctx.restore();
  }

  // Each league's contours from R: the outer (nine in ten) line dashed, the core (half) solid, each on a paper halo
  // so it reads over the dots. Labels sit at the top of each league's core, nudged apart when they collide.
  function drawContours() {
    function trace(poly) {
      var ws = arr(poly.w), hs = arr(poly.h);
      ctx.beginPath();
      for (var k = 0; k < ws.length; k++) { var x = px(ws[k]), y = py(hs[k]); if (k) ctx.lineTo(x, y); else ctx.moveTo(x, y); }
    }
    ctx.lineJoin = "round"; ctx.lineCap = "round";
    [["outer", 3.6, 1.5, [5, 4]], ["core", 5, 2.5, []]].forEach(function (lev) {
      contours.forEach(function (c) {
        var col = D.sports[D.lsport[c.lg]].color;
        arr(c[lev[0]]).forEach(function (poly) {
          trace(poly);
          ctx.setLineDash([]); ctx.strokeStyle = "rgba(245,241,230,.92)"; ctx.lineWidth = lev[1]; ctx.stroke();
          ctx.setLineDash(lev[3]); ctx.strokeStyle = col; ctx.lineWidth = lev[2]; ctx.stroke();
        });
      });
    });
    ctx.setLineDash([]);
    var labels = contours.map(function (c) {
      var best = null;
      arr(c.core).forEach(function (poly) { var hs = arr(poly.h), ws = arr(poly.w); for (var k = 0; k < hs.length; k++) if (!best || hs[k] > best.h) best = { h: hs[k], w: ws[k] }; });
      return best && { x: px(best.w), y: py(best.h) - 9, text: D.leagues[c.lg], col: D.sports[D.lsport[c.lg]].color };
    }).filter(Boolean).sort(function (a, b) { return a.y - b.y; });
    ctx.font = "600 10px IBM Plex Mono, ui-monospace, monospace"; ctx.textBaseline = "middle"; ctx.textAlign = "left";
    var placed = [], steps = [0, -18, 18, -36, 36, -54, 54, -72];
    labels.forEach(function (l) {
      var w = ctx.measureText(l.text).width + 22, x = Math.max(box.left + 4, Math.min(box.left + box.width - w - 4, l.x - w / 2)), y = l.y;
      for (var t = 0; t < steps.length; t++) {             // the nearest free spot, above or below the line's top
        var yy = Math.max(box.top + 10, l.y + steps[t]);
        if (!placed.some(function (q) { return x < q.x + q.w + 4 && q.x < x + w + 4 && Math.abs(q.y - yy) < 18; })) { y = yy; break; }
      }
      placed.push({ x: x, y: y, w: w });
      if (Math.abs(y - l.y) > 10) {                        // moved: a leader back to the top of its line
        ctx.strokeStyle = l.col; ctx.lineWidth = 1; ctx.beginPath(); ctx.moveTo(l.x, l.y + 9); ctx.lineTo(Math.max(x + 6, Math.min(x + w - 6, l.x)), y + (l.y > y ? 8 : -8)); ctx.stroke();
      }
      ctx.fillStyle = "rgba(251,249,243,.96)"; roundRect(ctx, x, y - 8, w, 16, 3); ctx.fill();
      ctx.strokeStyle = "rgba(29,38,34,.25)"; ctx.lineWidth = 1; ctx.stroke();
      ctx.strokeStyle = l.col; ctx.lineWidth = 2.5; ctx.beginPath(); ctx.moveTo(x + 5, y); ctx.lineTo(x + 13, y); ctx.stroke();
      ctx.fillStyle = INK; ctx.fillText(l.text, x + 17, y + 0.5);
    });
  }

  // Two tape measures: height up the left, weight along the bottom, numbers in the display units
  function drawTapes(X, Y, tx, ty, metric) {
    var mono = "IBM Plex Mono, ui-monospace, monospace", bottom = H - TH;
    // the height tape
    var g = ctx.createLinearGradient(0, 0, TW, 0); g.addColorStop(0, TAPE_LO); g.addColorStop(0.18, TAPE); g.addColorStop(1, TAPE);
    ctx.fillStyle = g; ctx.fillRect(0, 0, TW, bottom);
    vline(ctx, TW - 1, 0, bottom, "rgba(29,38,34,.55)", 1);
    ctx.save(); ctx.beginPath(); ctx.rect(0, 0, TW, bottom); ctx.clip();
    each(Y.lo, Y.hi, ty.minor, function (u) {
      var y = Y.at(u), big = isMult(u, ty.major), mid = !big && isMult(u, ty.major / 2);
      hline(ctx, TW - (big ? 12 : mid ? 8 : 5), TW, y, INK, 1);
    });
    ctx.textAlign = "right"; ctx.textBaseline = "middle";
    each(Y.lo, Y.hi, ty.major, function (u) {
      var y = Y.at(u); if (y < 6 || y > bottom - 6) return;
      var foot = !metric && isMult(u, 12), red = metric ? isMult(u, 50) : foot;
      ctx.font = (red ? "600 " : "500 ") + (W < 560 || !metric ? 9 : 10) + "px " + mono; ctx.fillStyle = red ? TAPE_RED : INK;
      ctx.fillText(metric ? f1(u) : foot ? (u / 12) + "′" : Math.floor(u / 12) + "′" + f1(u % 12) + "″", TW - 14, y);
    });
    ctx.restore();

    // the weight tape
    g = ctx.createLinearGradient(0, bottom, 0, H); g.addColorStop(0, TAPE); g.addColorStop(0.82, TAPE); g.addColorStop(1, TAPE_LO);
    ctx.fillStyle = g; ctx.fillRect(TW, bottom, W - TW, TH);
    hline(ctx, TW, W, bottom, "rgba(29,38,34,.55)", 1);
    ctx.save(); ctx.beginPath(); ctx.rect(TW, bottom, W - TW, TH); ctx.clip();
    each(X.lo, X.hi, tx.minor, function (u) {
      var x = X.at(u), big = isMult(u, tx.major), mid = !big && isMult(u, tx.major / 2);
      vline(ctx, x, bottom, bottom + (big ? 12 : mid ? 8 : 5), INK, 1);
    });
    ctx.textAlign = "center"; ctx.textBaseline = "alphabetic";
    each(X.lo, X.hi, tx.major, function (u) {
      var x = X.at(u); if (x < TW + 12 || x > W - 12) return;
      var red = isMult(u, metric ? 50 : 100);
      ctx.font = (red ? "600 " : "500 ") + (W < 560 ? 9 : 10) + "px " + mono; ctx.fillStyle = red ? TAPE_RED : INK;
      ctx.fillText(f1(u), x, H - 6);
    });
    ctx.restore();

    // the tape case in the corner, carrying the units
    ctx.fillStyle = INK; ctx.fillRect(0, bottom, TW, TH);
    ctx.fillStyle = TAPE; ctx.font = "600 8px " + mono; ctx.textAlign = "center"; ctx.textBaseline = "middle";
    ctx.fillText(metric ? "CM" : "FT", TW / 2, bottom + TH / 2 - 5); ctx.fillText(metric ? "KG" : "LB", TW / 2, bottom + TH / 2 + 6);
  }

  function buildIndex() {
    var b = new Map(), size = index.size, v = S.view;
    for (var j = 0; j < N; j++) {
      var xw = dw(j), yh = dh(j);
      if (!vis[j] || xw < v.x0 || xw > v.x1 || yh < v.y0 || yh > v.y1) continue;
      var x = px(xw), y = py(yh), k = Math.floor(x / size) + ":" + Math.floor(y / size);
      if (!b.has(k)) b.set(k, []); b.get(k).push({ j: j, x: x, y: y });
    }
    index.b = b;
  }
  // The nearest visible point within reach. Exact ties go to the first by name; the card lists everyone on the dot.
  function hitTest(x, y, radius) {
    var size = index.size, bx = Math.floor(x / size), by = Math.floor(y / size), reach = Math.ceil(radius / size), best = null, dist = radius * radius;
    for (var a = -reach; a <= reach; a++) for (var c = -reach; c <= reach; c++) {
      var cell = index.b.get((bx + a) + ":" + (by + c)); if (!cell) continue;
      for (var n = 0; n < cell.length; n++) {
        var it = cell[n], d = (it.x - x) * (it.x - x) + (it.y - y) * (it.y - y);
        if (d < dist || (d === dist && best && it.j < best.j)) { dist = d; best = it; }
      }
    }
    return best;
  }

  function drawOverlay() {
    octx.setTransform(dpr, 0, 0, dpr, 0, 0); octx.clearRect(0, 0, W, H);
    if (!ready) return;
    var a = S.hovered >= 0 ? S.hovered : S.pinned;
    octx.save(); octx.beginPath(); octx.rect(box.left, box.top, box.width, box.height); octx.clip();
    if (S.pinned >= 0 && S.pinned !== a) ring(S.pinned, false);
    if (a >= 0) { cross(px(dw(a)), py(dh(a)), "rgba(29,38,34,.42)"); ring(a, true); }
    octx.restore();
    if (a >= 0) readings(D.h[a], D.w[a], D.hin[a], D.lb[a], TAPE_RED, S.spread);
    else if (S.me) readings(S.me.h, S.me.w, S.me.h / CM_IN, S.me.w / KG_LB, INK);
  }
  function cross(x, y, col) {
    octx.setLineDash([3, 4]); octx.strokeStyle = col; octx.lineWidth = 1; octx.beginPath();
    octx.moveTo(box.left, Math.round(y) + 0.5); octx.lineTo(x, Math.round(y) + 0.5);
    octx.moveTo(Math.round(x) + 0.5, y); octx.lineTo(Math.round(x) + 0.5, box.top + box.height); octx.stroke(); octx.setLineDash([]);
  }
  function ring(j, primary) {
    var x = px(dw(j)), y = py(dh(j));
    if (!inside(x, y)) return;
    octx.fillStyle = PAPER; octx.beginPath(); octx.arc(x, y, primary ? 7.5 : 6, 0, 6.2832); octx.fill();
    octx.strokeStyle = color[j]; octx.lineWidth = 2; octx.stroke();
    octx.fillStyle = color[j]; octx.beginPath(); octx.arc(x, y, primary ? 3.6 : 3, 0, 6.2832); octx.fill();
  }
  // A reading on each tape: a hairline across the tape and the value in a tag
  function readings(h, w, hin, lb, line, band) {
    var s = sizeOf(h, w, hin, lb), y = py(h), x = px(w), bottom = H - TH;
    octx.font = "600 10px IBM Plex Mono, ui-monospace, monospace"; octx.textBaseline = "middle"; octx.textAlign = "center";
    if (band) {                                         // the listing: half an inch and half a pound either side
      octx.fillStyle = "rgba(184,41,31,.2)";
      var y0 = Math.max(0, py(h + CM_IN / 2)), y1 = Math.min(bottom, py(h - CM_IN / 2));
      if (y1 > y0) octx.fillRect(0, y0, TW, y1 - y0);
      var x0 = Math.max(TW, px(w - KG_LB / 2)), x1 = Math.min(W, px(w + KG_LB / 2));
      if (x1 > x0) octx.fillRect(x0, bottom, x1 - x0, TH);
    }
    if (y >= box.top && y <= box.top + box.height) {
      hline(octx, 0, TW, y, line, 1.5);
      var tw = Math.min(TW - 4, octx.measureText(s.h).width + 8), ty = Math.max(1, Math.min(bottom - 17, y - 8));
      octx.fillStyle = INK; roundRect(octx, 2, ty, tw, 16, 3); octx.fill();
      octx.fillStyle = PAPER; octx.fillText(s.h, 2 + tw / 2, ty + 8.5);
    }
    if (x >= box.left && x <= box.left + box.width) {
      vline(octx, x, bottom, H, line, 1.5);
      var label = s.w + " " + s.wu, lw = octx.measureText(label).width + 10, lx = Math.max(TW + 2, Math.min(W - lw - 2, x - lw / 2));
      octx.fillStyle = INK; roundRect(octx, lx, bottom + 7, lw, 16, 3); octx.fill();
      octx.fillStyle = PAPER; octx.fillText(label, lx + lw / 2, bottom + 15.5);
    }
  }
  function roundRect(c, x, y, w, h, r) {
    c.beginPath(); c.moveTo(x + r, y); c.arcTo(x + w, y, x + w, y + h, r); c.arcTo(x + w, y + h, x, y + h, r);
    c.arcTo(x, y + h, x, y, r); c.arcTo(x, y, x + w, y, r); c.closePath();
  }

  // ---- hover, tooltip, pin --------------------------------------------------------------------------------
  function hoverAt(x, y) {
    if (!ready || !inside(x, y)) { clearHover(); return; }
    var hit = hitTest(x, y, finePointer.matches ? 11 : 16), j = hit ? hit.j : -1;
    if (j !== S.hovered) { S.hovered = j; drawOverlay(); }
    if (j >= 0) showTip(j, x, y); else hideTip();
    canvas.style.cursor = j >= 0 ? "pointer" : zoom() > 1.001 ? "grab" : "crosshair";
  }
  function clearHover() { if (S.hovered >= 0) { S.hovered = -1; drawOverlay(); } hideTip(); }
  function hideTip() { if (tip) tip.hidden = true; }
  function showTip(j, x, y) {
    var s = sizeOfPlayer(j), g = groups.get(key(j)), n = g ? g.length : 1;
    tip.innerHTML = '<p class="tip__lg">' + dot(color[j]) + esc(D.leagues[D.lg[j]]) + " · " + esc(D.code[j]) + '</p>' +
      '<p class="tip__name">' + esc(D.name[j]) + '</p><p class="tip__team">' + esc(D.teams[D.team[j]]) + '</p>' +
      '<p class="tip__reads"><b>' + s.h + '</b>' + (s.hu ? "<small>" + s.hu + "</small>" : "") + '<b>' + s.w + '</b><small>' + s.wu + '</small></p>' +
      '<p class="tip__foot">' + (n > 1 ? n + " players listed at this size · click to see them all" : S.pinned === j ? "Pinned" : "Click to pin") + '</p>';
    tip.hidden = false;
    var tw = tip.offsetWidth, th = tip.offsetHeight, left = x + 16, top = y - th - 14;
    if (left + tw > W - 8) left = x - tw - 16;
    if (top < 8) top = y + 16;
    tip.style.transform = "translate(" + Math.max(8, Math.min(left, W - tw - 8)) + "px," + Math.max(8, Math.min(top, H - th - 8)) + "px)";
  }
  function setPin(j, tell) {
    S.pinned = j; S.hovered = -1; hideTip(); S.open = false; renderFinds();
    if (j >= 0) {
      if (!inside(px(dw(j)), py(dh(j)))) S.view = copy(BASE);       // a deep zoom never hides the player you asked for
      var s = sizeOfPlayer(j);
      $("sr").textContent = "Pinned " + D.name[j] + ", " + D.teams[D.team[j]] + ", " + s.h + " " + s.hu + ", " + s.w + " " + s.wu + ".";
    }
    schedule(); pinbar();
    if (tell) event("pin", { i: j + 1, t: Date.now() });
  }
  // On narrow screens the card is below the grid, so a bar docked to the grid says who is pinned
  function pinbar() {
    var bar = $("pinbar"), j = S.pinned;
    if (j < 0 || window.innerWidth >= NARROW) { bar.hidden = true; return; }
    var s = sizeOfPlayer(j);
    bar.innerHTML = dot(color[j]) + '<span class="pinbar__who"><b>' + esc(D.name[j]) + '</b></span><span class="pinbar__size">' + esc(D.leagues[D.lg[j]]) + " · " +
      s.h + (s.hu ? " " + s.hu : "") + " · " + s.w + " " + s.wu + '</span><span class="pinbar__go">Card ↓</span>';
    bar.setAttribute("aria-label", D.name[j] + ": show the card");
    bar.hidden = false;
  }

  // ---- units, sports, search ------------------------------------------------------------------------------
  function setUnits(u, tell) {
    S.units = u === "imperial" ? "imperial" : "metric";
    document.querySelectorAll("[data-units]").forEach(function (b) { var on = b.dataset.units === S.units; b.classList.toggle("is-on", on); b.setAttribute("aria-pressed", on); });
    axY.textContent = S.units === "metric" ? "Height · cm" : "Height · ft and in"; axX.textContent = S.units === "metric" ? "Weight · kg" : "Weight · lb";
    if (tell && window.Shiny && Shiny.setInputValue) Shiny.setInputValue("units", S.units);
    hideTip(); schedule(); pinbar();
  }
  function renderFinds() {
    var list = $("finds"), q = $("q");
    if (!list) return;
    if (!S.q || !S.open) { list.hidden = true; q.setAttribute("aria-expanded", "false"); q.removeAttribute("aria-activedescendant"); return; }
    S.active = Math.max(0, Math.min(S.active, S.matches.length - 1));
    list.innerHTML = S.matches.length ? S.matches.map(function (j, k) {
      return '<div id="find-' + k + '" class="find__opt' + (k === S.active ? " is-on" : "") + '" role="option" aria-selected="' + (k === S.active) + '" data-find="' + k + '">' +
        dot(color[j]) + '<span><b>' + esc(D.name[j]) + '</b><small>' + esc(D.teams[D.team[j]]) + " · " + esc(D.code[j]) + '</small></span><em>' + esc(D.leagues[D.lg[j]]) + '</em></div>';
    }).join("") + (S.total > S.matches.length ? '<div class="find__more">' + S.matches.length + " of " + nf.format(S.total) + " matches, all on the grid. Keep typing to narrow it down.</div>" : "")
      : '<div class="find__none">' + (S.narrowed ? "Nobody by that name among the leagues in view." : "No player or team by that name.") + '</div>';
    list.hidden = false; q.setAttribute("aria-expanded", "true");
    if (S.matches.length) q.setAttribute("aria-activedescendant", "find-" + S.active); else q.removeAttribute("aria-activedescendant");
  }
  function boardMessage() {
    if (S.total > 0) { msg.hidden = true; return; }
    msg.innerHTML = "<strong>Nobody here</strong><span>No one matches that name, league and position.</span>" +
      '<button type="button" data-reset>Show everyone</button>';
    msg.hidden = false;
  }
  function nudge(text) {
    var el = $("board-hint");
    el.textContent = text; el.classList.add("is-on");
    clearTimeout(hintTimer); hintTimer = setTimeout(function () { el.classList.remove("is-on"); }, 1600);
  }

  function allLeagues() { return Array.prototype.map.call(document.querySelectorAll(".chip[data-league]:not([data-league=all])"), function (b) { return b.dataset.league; }); }
  function applyChips(on) {
    var every = allLeagues(), all = on.length >= every.length;
    document.querySelectorAll(".chip[data-league]").forEach(function (b) {
      var l = b.dataset.league, st = l === "all" ? (all ? "on" : "off") : all ? "all" : on.indexOf(l) >= 0 ? "on" : "off";
      b.dataset.state = st; b.setAttribute("aria-pressed", st === "on");
    });
    chipsOn = all ? null : on.slice();
  }
  function nextLeagues(l) {
    var every = allLeagues(), cur = chipsOn || every;
    if (l === "all") return every;
    if (!chipsOn) return [l];
    if (cur.indexOf(l) >= 0) return cur.length === 1 ? every : cur.filter(function (x) { return x !== l; });
    return every.filter(function (x) { return x === l || cur.indexOf(x) >= 0; });
  }
  function copyLink(btn) {
    var url = location.href.replace(/\/_w_[0-9a-f]+\//, "/"), label = btn.dataset.label || (btn.dataset.label = btn.textContent);
    function done(ok) {
      btn.textContent = ok ? "Link copied" : "Copy the address bar to share"; btn.classList.toggle("is-done", ok);
      setTimeout(function () { btn.textContent = label; btn.classList.remove("is-done"); }, 2200);
    }
    function fallback() {
      var ta = document.createElement("textarea"), ok = false; ta.value = url; ta.setAttribute("readonly", ""); ta.style.cssText = "position:fixed;opacity:0";
      document.body.appendChild(ta); ta.select(); try { ok = document.execCommand("copy"); } catch (err) { ok = false; } ta.remove(); return ok;
    }
    if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(url).then(function () { done(true); }, function () { done(fallback()); });
    else done(fallback());
  }
  // The grid fills what's left of the first screen on wide layouts, so both tapes show without scrolling
  function fitBoard() {
    if (document.fullscreenElement || window.innerWidth < NARROW) { board.style.height = ""; return; }
    var top = board.getBoundingClientRect().top + window.scrollY, foot = $("how-to").offsetHeight + 28;
    board.style.height = Math.round(Math.max(440, Math.min(900, window.innerHeight - top - foot))) + "px";
  }

  // ---- messages from R ---------------------------------------------------------------------------------------
  var handlers = {
    vis: function (m) {
      if (m.all) vis.fill(1); else { vis.fill(0); arr(m.idx).forEach(function (j) { vis[j] = 1; }); }
      visList = []; groups = new Map();
      for (var j = 0; j < N; j++) if (vis[j]) { visList.push(j); var k = key(j); if (!groups.has(k)) groups.set(k, []); groups.get(k).push(j); }
      S.matches = arr(m.matches); S.q = !!m.q; S.total = m.total; S.narrowed = !!m.narrowed; S.active = 0; S.hovered = -1; hideTip();
      renderFinds(); boardMessage(); schedule();
      canvas.setAttribute("aria-label", "Height against weight, " + nf.format(S.total) + " athletes. With the grid focused, arrow keys step through players, plus and minus zoom, zero resets.");
    },
    chips: function (m) { applyChips(arr(m)); },
    pos_state: function (m) { var sel = $("pos"); sel.disabled = !m.on; sel.closest(".pick").classList.toggle("is-off", !m.on); },
    pin: function (m) { setPin(m.i, false); },
    me: function (m) {
      var was = S.me; S.me = m.on ? { h: m.h, w: m.w } : null;
      if (S.me && (!was || was.h !== S.me.h || was.w !== S.me.w) && !reduceMotion.matches) S.meAt = performance.now();
      if (S.me && !inside(px(S.me.w), py(S.me.h))) S.view = copy(BASE);
      schedule();
    },
    units: function (u) { setUnits(u, true); },
    contours: function (m) {
      S.contours = !!m.on; contours = m.on ? arr(m.leagues) : null;
      $("contours").setAttribute("aria-pressed", S.contours); $("contour-key").hidden = !S.contours;
      $("contour-key").classList.toggle("is-none", !!contours && contours.length === 0);   // say why there are no lines
      schedule();
    }
  };
  Object.keys(handlers).forEach(function (name) {
    Shiny.addCustomMessageHandler(name, function (m) { if (ready) handlers[name](m); else queue.push([name, m]); });
  });

  // ---- start -------------------------------------------------------------------------------------------------
  function init() {
    D = JSON.parse($("athletes").textContent); N = D.name.length;
    sportOf = D.lg.map(function (l) { return D.lsport[l]; });
    color = sportOf.map(function (s) { return D.sports[s].color; });
    var fills = D.sports.map(function (s) { var c = s.color; return "rgba(" + parseInt(c.slice(1, 3), 16) + "," + parseInt(c.slice(3, 5), 16) + "," + parseInt(c.slice(5, 7), 16) + ",.88)"; });
    fill = sportOf.map(function (s) { return fills[s]; });
    var dims = fills.map(function (f) { return f.replace(",.88)", ",.42)"); });
    fillDim = sportOf.map(function (s) { return dims[s]; });
    drawOrder = new Int32Array(N); for (var j = 0; j < N; j++) drawOrder[D.z[j]] = j;
    vis = new Uint8Array(N).fill(1); S.total = N;
    for (j = 0; j < N; j++) { var k = key(j); if (!groups.has(k)) groups.set(k, []); groups.get(k).push(j); visList.push(j); }

    canvas = $("plot"); overlay = $("overlay"); board = $("board"); tip = $("tip"); msg = $("board-msg");
    ctx = canvas.getContext("2d"); octx = overlay.getContext("2d");
    axY = document.createElement("span"); axY.className = "axis axis--y"; axY.setAttribute("aria-hidden", "true");
    axX = document.createElement("span"); axX.className = "axis axis--x"; axX.setAttribute("aria-hidden", "true");
    var hint = document.createElement("span"); hint.className = "board__hint"; hint.id = "board-hint"; hint.setAttribute("aria-hidden", "true");
    board.appendChild(axY); board.appendChild(axX); board.appendChild(hint);
    ready = true; setUnits("metric", false);
    fitBoard(); new ResizeObserver(resize).observe(board); resize();
    window.addEventListener("resize", function () { fitBoard(); pinbar(); });
    if (document.fonts) { document.fonts.ready.then(function () { fitBoard(); schedule(); }); document.fonts.load(HAND).then(schedule); }
    queue.forEach(function (q) { handlers[q[0]](q[1]); }); queue = [];
    bind();
  }

  function local(e) { var r = canvas.getBoundingClientRect(); return { x: e.clientX - r.left, y: e.clientY - r.top }; }

  function bind() {
    canvas.addEventListener("pointermove", function (e) {
      var p = local(e);
      if (drag && drag.id === e.pointerId) {
        var dx = p.x - drag.x, dy = p.y - drag.y;
        if (!drag.moved && Math.hypot(dx, dy) > 4) drag.moved = true;
        if (drag.moved && zoom() > 1.001) { S.view = panFrom(drag.view, dx, dy); S.hovered = -1; hideTip(); board.classList.add("is-panning"); schedule(); }
        return;
      }
      if (e.pointerType !== "touch") hoverAt(p.x, p.y);
    });
    canvas.addEventListener("pointerdown", function (e) {
      if (e.button !== 0) return;
      var p = local(e); if (!inside(p.x, p.y)) return;
      drag = { id: e.pointerId, x: p.x, y: p.y, view: copy(S.view), moved: false };
      canvas.setPointerCapture(e.pointerId);
      S.open = false; renderFinds();
    });
    canvas.addEventListener("pointerup", function (e) {
      if (!drag || drag.id !== e.pointerId) return;
      var p = local(e), moved = drag.moved;
      drag = null; board.classList.remove("is-panning");
      if (canvas.hasPointerCapture(e.pointerId)) canvas.releasePointerCapture(e.pointerId);
      if (moved) return;
      var hit = hitTest(p.x, p.y, e.pointerType === "touch" ? 18 : 11);
      if (hit) { setPin(hit.j, true); if (e.pointerType !== "touch") showTip(hit.j, p.x, p.y); }
      else if (S.pinned >= 0) setPin(-1, true);
    });
    canvas.addEventListener("pointercancel", function () { drag = null; board.classList.remove("is-panning"); clearHover(); });
    canvas.addEventListener("pointerleave", function (e) { if (e.pointerType !== "touch" && !drag) clearHover(); });
    // A pinch always zooms. A plain wheel zooms once the grid has focus, so the page still scrolls past it.
    canvas.addEventListener("wheel", function (e) {
      var p = local(e); if (!inside(p.x, p.y)) return;
      if (!e.ctrlKey && document.activeElement !== canvas) { nudge("Click the grid first, or pinch, to zoom"); return; }
      e.preventDefault();
      var dy = Math.max(-150, Math.min(150, e.deltaY));
      zoomAt(Math.exp(-dy * (e.ctrlKey ? 0.012 : 0.003)), p.x, p.y);
    }, { passive: false });
    canvas.addEventListener("keydown", function (e) {
      if (e.key === "+" || e.key === "=") { e.preventDefault(); zoomAt(1.5); }
      else if (e.key === "-" || e.key === "_") { e.preventDefault(); zoomAt(1 / 1.5); }
      else if (e.key === "0") { e.preventDefault(); S.view = copy(BASE); schedule(); }
      else if (e.key === "Escape") { if (S.pinned >= 0) setPin(-1, true); }
      else if (/^Arrow/.test(e.key) && visList.length) {
        e.preventDefault();
        var at = visList.indexOf(S.pinned), step = e.key === "ArrowRight" || e.key === "ArrowDown" ? 1 : -1;
        var next = visList[((at < 0 ? (step > 0 ? -1 : 0) : at) + step + visList.length) % visList.length];
        setPin(next, true);
        var x = px(dw(next)), y = py(dh(next)); if (inside(x, y)) showTip(next, x, y);
      }
    });

    $("spread").addEventListener("click", function () {
      S.spread = !S.spread; this.setAttribute("aria-checked", S.spread); S.hovered = -1; hideTip(); schedule();
    });
    $("contours").addEventListener("click", function () {
      S.contours = !S.contours; this.setAttribute("aria-pressed", S.contours);
      if (!S.contours) { $("contour-key").hidden = true; schedule(); }
      if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue("contours", S.contours);
    });
    $("zoom-in").addEventListener("click", function () { zoomAt(1.5); });
    $("zoom-out").addEventListener("click", function () { zoomAt(1 / 1.5); });
    $("zoom-reset").addEventListener("click", function () { S.view = copy(BASE); S.hovered = -1; hideTip(); schedule(); });
    $("full").addEventListener("click", function () {
      var wall = $("wall");
      if (document.fullscreenElement) document.exitFullscreen();
      else if (wall.requestFullscreen) wall.requestFullscreen().catch(function () { nudge("Full screen isn't available here"); });
      else nudge("Full screen isn't available in this browser");
    });
    document.addEventListener("fullscreenchange", function () {
      $("full").setAttribute("aria-label", document.fullscreenElement ? "Leave full screen" : "Full screen"); fitBoard();
    });

    document.addEventListener("click", function (e) {
      var t = e.target.closest ? e.target : e.target.parentElement;
      var b = t.closest("[data-pin]");
      if (b) { var j = Number(b.dataset.pin) - 1; setPin(j, true); if (j >= 0 && window.innerWidth < NARROW && b.closest(".card")) $("wall").scrollIntoView({ block: "start", behavior: reduceMotion.matches ? "auto" : "smooth" }); return; }
      var cp = t.closest("[data-copy]"); if (cp) { copyLink(cp); return; }
      if (t.closest("#pinbar")) { $("spotlight").scrollIntoView({ block: "start", behavior: reduceMotion.matches ? "auto" : "smooth" }); return; }
      if (t.closest("[data-reset]")) { event("reset", Date.now()); return; }
      if (t.closest("[data-clear-me]")) { event("me_clear", Date.now()); return; }
      var u = t.closest("[data-units]"); if (u) { setUnits(u.dataset.units, true); return; }
      var c = t.closest(".chip[data-league]");
      if (c) { applyChips(nextLeagues(c.dataset.league)); event("league_pick", { l: c.dataset.league, t: Date.now() }); return; }
      var f = t.closest("[data-find]"); if (f) { setPin(S.matches[Number(f.dataset.find)], true); return; }
      if (!t.closest(".find")) { S.open = false; renderFinds(); }
    });
    $("finds").addEventListener("mousedown", function (e) { e.preventDefault(); });

    var q = $("q");
    q.addEventListener("input", function () { S.open = true; S.active = 0; });
    q.addEventListener("focus", function () { S.open = true; renderFinds(); });
    q.addEventListener("keydown", function (e) {
      if (e.key === "ArrowDown" || e.key === "ArrowUp") {
        e.preventDefault(); S.open = true; S.active += e.key === "ArrowDown" ? 1 : -1; renderFinds();
        var o = $("find-" + S.active); if (o) o.scrollIntoView({ block: "nearest" });
      } else if (e.key === "Enter" && S.open && S.matches.length) {
        e.preventDefault(); setPin(S.matches[S.active], true); q.blur(); canvas.focus({ preventScroll: true });
      } else if (e.key === "Escape") { S.open = false; renderFinds(); }
    });
    document.addEventListener("keydown", function (e) {
      var tag = document.activeElement && document.activeElement.tagName;
      if (e.key === "/" && tag !== "INPUT" && tag !== "SELECT" && tag !== "TEXTAREA") { e.preventDefault(); q.focus(); }
    });
    // Choosing your own size takes the pin off, so your card shows. Shiny's own updates to these selects fire
    // jQuery events, which never reach a native listener, so only the person's own choice lands here.
    document.addEventListener("change", function (e) {
      if (e.target.id !== "me_h" && e.target.id !== "me_w") return;
      if (S.pinned >= 0) setPin(-1, true);
      if ($("me_h").value && $("me_w").value && window.innerWidth < NARROW)
        setTimeout(function () { $("wall").scrollIntoView({ block: "start", behavior: reduceMotion.matches ? "auto" : "smooth" }); }, 250);
    });
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", init); else init();
})();
