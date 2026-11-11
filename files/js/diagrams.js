/* ==========================================================================
   Chao-UI Analytical Diagram Kit (diagrams.js)
   Zero-dependency ES module for empirical & mathematical HTML5 <canvas> figures.

   Design contract (per chao-ui/docs/experience-principles.md):
   - Reads live CSS tokens (--pf-*) so light/dark themes match natively.
   - Enforces chao-ui 0px square geometry, hairline axes, and IBM Plex + Inconsolata type.
   - Uses IntersectionObserver to start episodes in view and pause off-screen loops.
   - Respects prefers-reduced-motion with a complete explanatory frame, while
     preserving manual Pause / Replay and pointer scrubbing.
   ========================================================================== */

export const clamp = (x, a, b) => Math.max(a, Math.min(b, x));
export const lerp = (a, b, t) => a + (b - a) * t;
export const ease = (t) => {
  t = clamp(t, 0, 1);
  return t * t * (3 - 2 * t);
};
export const seg = (t, a, b) => (b <= a ? (t >= b ? 1 : 0) : clamp((t - a) / (b - a), 0, 1));

export const FONTS = {
  sans: '12px "IBM Plex Sans", system-ui, -apple-system, sans-serif',
  sansSmall: '11px "IBM Plex Sans", system-ui, -apple-system, sans-serif',
  sansBold: '600 12px "IBM Plex Sans", system-ui, -apple-system, sans-serif',
  mono: '12px "Inconsolata", "IBM Plex Mono", ui-monospace, monospace',
  monoSmall: '11px "Inconsolata", "IBM Plex Mono", ui-monospace, monospace',
  monoBold: '600 11.5px "Inconsolata", "IBM Plex Mono", ui-monospace, monospace',
  math: 'italic 14px "IBM Plex Serif", "STIX Two Text", Georgia, serif',
  mathBold: 'italic 600 14px "IBM Plex Serif", "STIX Two Text", Georgia, serif',
};

export function parseRgb(str, fallback = [31, 29, 26]) {
  const s = (str || "").trim();
  if (s.startsWith("#")) {
    const hex = s.slice(1);
    if (hex.length === 3) {
      return hex.split("").map((c) => parseInt(c + c, 16));
    }
    if (hex.length >= 6) {
      return [
        parseInt(hex.slice(0, 2), 16),
        parseInt(hex.slice(2, 4), 16),
        parseInt(hex.slice(4, 6), 16),
      ];
    }
  }
  const m = s.match(/rgba?\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)/i);
  if (m) return [Number(m[1]), Number(m[2]), Number(m[3])];
  return fallback;
}

export function rgba(rgbTuple, a = 1) {
  const [r, g, b] = rgbTuple;
  return a >= 1 ? `rgb(${Math.round(r)},${Math.round(g)},${Math.round(b)})` : `rgba(${Math.round(r)},${Math.round(g)},${Math.round(b)},${a})`;
}

export function mixRgb(c1, c2, t) {
  const f = clamp(t, 0, 1);
  return [lerp(c1[0], c2[0], f), lerp(c1[1], c2[1], f), lerp(c1[2], c2[2], f)];
}

export function palette(el = document.documentElement) {
  const cs = getComputedStyle(el);
  const read = (name, fb) => cs.getPropertyValue(name).trim() || fb;
  const bg = read("--pf-essay-bg", read("--pf-bg", "#ffffff"));
  const fg = read("--pf-vis-ink", read("--pf-fg", "#1f1d1a"));
  const muted = read("--pf-vis-muted", read("--pf-muted", "#6e6a63"));
  const grid = read("--pf-vis-grid", read("--pf-border", "#e6e2dc"));
  const surface = read("--pf-essay-surface", read("--pf-surface", "#f7f6f2"));
  const slateHex = read("--pf-vis-slate", "#4f7d96");
  const terraHex = read("--pf-vis-terracotta", "#c85a40");
  const ochreHex = read("--pf-vis-ochre", "#c68d28");
  const stoneHex = read("--pf-vis-stone", "#cbc6be");

  const slateRgb = parseRgb(slateHex, [79, 125, 150]);
  const terraRgb = parseRgb(terraHex, [200, 90, 64]);
  const ochreRgb = parseRgb(ochreHex, [198, 141, 40]);
  const stoneRgb = parseRgb(stoneHex, [203, 198, 190]);
  const fgRgb = parseRgb(fg, [31, 29, 26]);

  return {
    bg,
    fg,
    muted,
    grid,
    surface,
    slate: slateHex,
    terracotta: terraHex,
    ochre: ochreHex,
    stone: stoneHex,
    slateRgb,
    terraRgb,
    ochreRgb,
    stoneRgb,
    fgRgb,
    slateAlpha: (a) => rgba(slateRgb, a),
    terraAlpha: (a) => rgba(terraRgb, a),
    ochreAlpha: (a) => rgba(ochreRgb, a),
    fgAlpha: (a) => rgba(fgRgb, a),
  };
}

export function label(ctx, text, x, y, opts = {}) {
  ctx.font = opts.font || FONTS.sans;
  ctx.fillStyle = opts.color || "#1f1d1a";
  ctx.textAlign = opts.align || "left";
  ctx.textBaseline = opts.baseline || "middle";
  ctx.fillText(text, x, y);
}

/** Square-swatch chao-ui legend (0px corners). */
export function legend(ctx, items, cx, y, P) {
  ctx.font = FONTS.sans;
  ctx.textBaseline = "middle";
  const sw = 10;
  const gap = 6;
  const pad = 18;
  const widths = items.map((it) => sw + gap + ctx.measureText(it.label).width);
  const total = widths.reduce((a, b) => a + b, 0) + pad * Math.max(0, items.length - 1);
  let x = cx - total / 2;
  items.forEach((it, i) => {
    ctx.fillStyle = it.color;
    ctx.fillRect(Math.round(x), Math.round(y - sw / 2), sw, sw);
    ctx.fillStyle = P.muted;
    ctx.textAlign = "left";
    ctx.fillText(it.label, x + sw + gap, y + 0.5);
    x += widths[i] + pad;
  });
}

/** Architectural 0px chao-ui framed panel with title & right-aligned mono metadata. */
export function panel(ctx, x, y, w, h, title, rightMeta, P) {
  ctx.fillStyle = P.bg;
  ctx.fillRect(x, y, w, h);
  ctx.strokeStyle = P.grid;
  ctx.lineWidth = 1;
  ctx.strokeRect(Math.round(x) + 0.5, Math.round(y) + 0.5, Math.round(w) - 1, Math.round(h) - 1);
  ctx.beginPath();
  ctx.moveTo(x, Math.round(y + 26) + 0.5);
  ctx.lineTo(x + w, Math.round(y + 26) + 0.5);
  ctx.stroke();
  if (title) label(ctx, title, x + 10, y + 13.5, { color: P.fg, font: FONTS.sansBold });
  if (rightMeta) label(ctx, rightMeta, x + w - 10, y + 13.5, { color: P.muted, font: FONTS.monoSmall, align: "right" });
}

export function arrow(ctx, x0, y0, x1, y1, color, lw = 1.6, head = 7) {
  const len = Math.hypot(x1 - x0, y1 - y0);
  if (len < 1) return;
  const a = Math.atan2(y1 - y0, x1 - x0);
  ctx.strokeStyle = color;
  ctx.fillStyle = color;
  ctx.lineWidth = lw;
  ctx.beginPath();
  ctx.moveTo(x0, y0);
  ctx.lineTo(x1 - Math.cos(a) * head * 0.78, y1 - Math.sin(a) * head * 0.78);
  ctx.stroke();
  ctx.beginPath();
  ctx.moveTo(x1, y1);
  ctx.lineTo(x1 - head * Math.cos(a - 0.38), y1 - head * Math.sin(a - 0.38));
  ctx.lineTo(x1 - head * Math.cos(a + 0.38), y1 - head * Math.sin(a + 0.38));
  ctx.closePath();
  ctx.fill();
}

/** Clip a 2D convex polygon against half-space a[0]*x + a[1]*y <= b. */
export function clipHalfspace(poly, a, b) {
  const evalPt = (p) => a[0] * p[0] + a[1] * p[1] - b;
  const out = [];
  for (let i = 0; i < poly.length; i++) {
    const p = poly[i];
    const q = poly[(i + 1) % poly.length];
    const fp = evalPt(p);
    const fq = evalPt(q);
    if (fp <= 1e-9) out.push(p);
    if ((fp < -1e-9 && fq > 1e-9) || (fp > 1e-9 && fq < -1e-9)) {
      const s = fp / (fp - fq);
      out.push([lerp(p[0], q[0], s), lerp(p[1], q[1], s)]);
    }
  }
  return out;
}

export function feasiblePolygon(box, constraints) {
  const [x0, x1, y0, y1] = box;
  let poly = [
    [x0, y0],
    [x1, y0],
    [x1, y1],
    [x0, y1],
  ];
  for (const c of constraints) {
    poly = clipHalfspace(poly, c.a, c.b);
    if (poly.length === 0) break;
  }
  return poly;
}

export function lineInBox(a, b, box) {
  const [x0, x1, y0, y1] = box;
  const pts = [];
  const [a1, a2] = a;
  if (Math.abs(a2) > 1e-9) {
    for (const x of [x0, x1]) {
      const y = (b - a1 * x) / a2;
      if (y >= y0 - 1e-6 && y <= y1 + 1e-6) pts.push([x, clamp(y, y0, y1)]);
    }
  }
  if (Math.abs(a1) > 1e-9) {
    for (const y of [y0, y1]) {
      const x = (b - a2 * y) / a1;
      if (x > x0 + 1e-6 && x < x1 - 1e-6) pts.push([clamp(x, x0, x1), y]);
    }
  }
  if (pts.length < 2) return null;
  pts.sort((p, q) => p[0] - q[0] || p[1] - q[1]);
  return [pts[0], pts[pts.length - 1]];
}

export function wrapMono(text, maxChars) {
  const out = [];
  for (const para of String(text).split("\n")) {
    if (para.length <= maxChars) {
      out.push(para);
      continue;
    }
    const indent = (para.match(/^\s*/) || [""])[0].slice(0, Math.floor(maxChars / 3));
    let line = "";
    for (const word of para.trim().split(/ +/)) {
      const cand = line ? `${line} ${word}` : `${indent}${word}`;
      if (cand.length <= maxChars) line = cand;
      else {
        if (line) out.push(line);
        line = `${indent}${word}`.slice(0, maxChars);
      }
    }
    if (line) out.push(line);
  }
  return out;
}

/**
 * Mount a responsive HiDPI canvas diagram.
 * spec: {
 *   height: (W, ctx) => number,
 *   cycle?: number,          // loop period in seconds (omit or 0 for static/pointer-only)
 *   reduceAt?: number,       // static timestamp in seconds under prefers-reduced-motion
 *   draw: (ctx, W, H, state) => void
 * }
 */
export function mountDiagram(canvas, spec) {
  const ctx = canvas.getContext("2d");
  const cycle = spec.cycle || 0;
  const reducedMotion = () =>
    !!(window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches);

  let W = 0;
  let H = 0;
  let inView = false;
  let paused = reducedMotion();
  let rafId = 0;
  let elapsedMs = paused && cycle > 0 ? (spec.reduceAt ?? cycle * 0.95) * 1000 : 0;
  let lastNow = null;

  const pointer = { x: 0, y: 0, normX: 0.5, normY: 0.5, active: false };

  function syncToggleButton() {
    const fig = canvas.closest("figure");
    const btn = fig && fig.querySelector("[data-diagram-toggle]");
    if (!btn) return;
    btn.textContent = paused ? "▶ Play" : "❚❚ Pause";
    btn.setAttribute("aria-pressed", String(paused));
  }

  function renderOnce() {
    if (W <= 0 || H <= 0) return;
    ctx.clearRect(0, 0, W, H);
    const P = palette(canvas);
    const t = cycle > 0 ? (elapsedMs / 1000) % cycle : elapsedMs / 1000;
    spec.draw(ctx, W, H, { t, elapsedMs, cycle, paused, pointer, P });
  }

  function tick(now) {
    rafId = 0;
    if (!inView) {
      lastNow = null;
      return;
    }
    if (!paused && cycle > 0) {
      if (lastNow !== null) elapsedMs += Math.min(100, Math.max(0, now - lastNow));
      lastNow = now;
    } else {
      lastNow = null;
    }
    renderOnce();
    if (!paused && cycle > 0 && inView) {
      rafId = window.requestAnimationFrame(tick);
    }
  }

  function schedule() {
    if (!rafId && inView && !paused && cycle > 0) {
      lastNow = null;
      rafId = window.requestAnimationFrame(tick);
    } else {
      renderOnce();
    }
  }

  function resize() {
    const dpr = Math.min(window.devicePixelRatio || 1, 2);
    W = Math.max(1, Math.floor(canvas.clientWidth || canvas.getBoundingClientRect().width || 640));
    H = Math.max(1, Math.round(spec.height(W, ctx)));
    canvas.style.height = `${H}px`;
    canvas.width = Math.round(W * dpr);
    canvas.height = Math.round(H * dpr);
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    renderOnce();
  }

  new ResizeObserver(resize).observe(canvas);
  new IntersectionObserver(
    (entries) => {
      const vis = entries.some((e) => e.isIntersecting);
      inView = vis;
      if (vis) schedule();
    },
    { threshold: 0.05 }
  ).observe(canvas);

  canvas.addEventListener("pointermove", (e) => {
    const r = canvas.getBoundingClientRect();
    pointer.x = e.clientX - r.left;
    pointer.y = e.clientY - r.top;
    pointer.normX = clamp(pointer.x / Math.max(1, r.width), 0, 1);
    pointer.normY = clamp(pointer.y / Math.max(1, r.height), 0, 1);
    pointer.active = true;
    if (paused || cycle === 0) renderOnce();
  });
  canvas.addEventListener("pointerleave", () => {
    pointer.active = false;
    if (paused || cycle === 0) renderOnce();
  });

  const fig = canvas.closest("figure");
  const toggleBtn = fig && fig.querySelector("[data-diagram-toggle]");
  if (toggleBtn) {
    toggleBtn.addEventListener("click", () => {
      paused = !paused;
      if (!paused && reducedMotion() && elapsedMs >= (spec.reduceAt ?? cycle * 0.95) * 990) {
        elapsedMs = 0;
      }
      syncToggleButton();
      schedule();
    });
  }

  window.addEventListener("chao-theme-change", renderOnce);
  if (document.fonts && document.fonts.ready) {
    document.fonts.ready.then(renderOnce).catch(() => {});
  }

  syncToggleButton();
  resize();
  return { redraw: renderOnce, pause: () => { paused = true; syncToggleButton(); }, play: () => { paused = false; syncToggleButton(); schedule(); } };
}
