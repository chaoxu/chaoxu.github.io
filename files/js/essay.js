/* ==========================================================================
   Chao-UI Exposition Runtime (essay.js)
   Progressive enhancement for:
     1. Light / Dark theme persistence ([data-pf-theme-toggle])
     2. Auto-generated & existing sticky TOC + scroll-spy (.pf-essay-toc)
     3. Collision-free Tufte margin sidenotes from Pandoc/Markdown footnotes
   ========================================================================== */

(function () {
  "use strict";

  const STORAGE_KEY = "chao-ui-theme";

  function preferredTheme() {
    try {
      const saved = localStorage.getItem(STORAGE_KEY);
      if (saved === "dark" || saved === "light") return saved;
    } catch (_) {}
    return window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches
      ? "dark"
      : "light";
  }

  function applyTheme(mode, persist) {
    const root = document.documentElement;
    if (mode === "dark") {
      root.setAttribute("data-theme", "dark");
    } else {
      root.removeAttribute("data-theme");
    }
    if (persist) {
      try {
        localStorage.setItem(STORAGE_KEY, mode);
      } catch (_) {}
    }
    document.querySelectorAll("[data-pf-theme-toggle]").forEach((btn) => {
      const isDark = mode === "dark";
      btn.setAttribute("aria-pressed", String(isDark));
      const short = btn.hasAttribute("data-pf-theme-short");
      btn.textContent = short ? (isDark ? "☼ Light" : "☾ Dark") : isDark ? "☼ Light Mode" : "☾ Dark Mode";
    });
    window.dispatchEvent(new CustomEvent("chao-theme-change", { detail: { theme: mode } }));
  }

  // Apply before paint if loaded synchronously, and wire buttons on DOM ready
  applyTheme(preferredTheme(), false);

  function initThemeControls() {
    applyTheme(preferredTheme(), false);
    document.querySelectorAll("[data-pf-theme-toggle]").forEach((btn) => {
      btn.addEventListener("click", () => {
        const next = document.documentElement.getAttribute("data-theme") === "dark" ? "light" : "dark";
        applyTheme(next, true);
      });
    });
    if (window.matchMedia) {
      window.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", (e) => {
        try {
          if (localStorage.getItem(STORAGE_KEY)) return;
        } catch (_) {}
        applyTheme(e.matches ? "dark" : "light", false);
      });
    }
  }

  function initTocAndScrollSpy() {
    const tocAside = document.querySelector(".pf-essay-toc");
    const prose = document.querySelector(".pf-essay-prose");
    if (!tocAside || !prose) return;

    let links = Array.from(tocAside.querySelectorAll('a[href^="#"]'));

    // Auto-build TOC when template leaves .pf-essay-toc empty (e.g. Hakyll posts)
    if (links.length === 0) {
      const headings = Array.from(
        prose.querySelectorAll("h1[id]:not(#article-title):not(.unlisted), h2[id]:not(.unlisted), h3[id]:not(.unlisted)")
      );
      if (headings.length < 2) return;

      const topLevel = Math.min(...headings.map((h) => Number(h.tagName.slice(1))));
      const nav = document.createElement("nav");
      nav.setAttribute("aria-label", "Table of contents");

      const label = document.createElement("div");
      label.className = "pf-essay-toc-label";
      label.textContent = document.documentElement.lang.startsWith("zh") ? "目录" : "Contents";
      nav.appendChild(label);

      const rootUl = document.createElement("ul");
      let currentSubUl = null;

      headings.forEach((h) => {
        const level = Number(h.tagName.slice(1));
        const a = document.createElement("a");
        a.href = "#" + h.id;
        const syncLabel = () => {
          const clone = h.cloneNode(true);
          clone.querySelectorAll("a.anchor").forEach((el) => el.remove());
          clone.querySelectorAll("[id]").forEach((el) => el.removeAttribute("id"));
          // Unwrap any nested <a> inside heading so TOC <a> stays valid HTML
          clone.querySelectorAll("a").forEach((nested) => {
            nested.replaceWith(...nested.childNodes);
          });
          a.innerHTML = clone.innerHTML.trim();
        };
        syncLabel();
        // Re-sync after deferred KaTeX auto-render finishes on headings
        window.addEventListener("load", syncLabel, { once: true });

        const li = document.createElement("li");
        li.appendChild(a);

        if (level <= topLevel) {
          rootUl.appendChild(li);
          currentSubUl = null;
        } else {
          if (!currentSubUl) {
            currentSubUl = document.createElement("ul");
            const lastTop = rootUl.lastElementChild || rootUl;
            lastTop.appendChild(currentSubUl);
          }
          currentSubUl.appendChild(li);
        }
      });

      nav.appendChild(rootUl);
      tocAside.appendChild(nav);
      links = Array.from(nav.querySelectorAll('a[href^="#"]'));
    }

    const byId = new Map();
    links.forEach((a) => {
      const id = decodeURIComponent(a.getAttribute("href").slice(1));
      if (id) byId.set(id, a);
    });

    const targets = Array.from(byId.keys())
      .map((id) => document.getElementById(id))
      .filter(Boolean);
    if (targets.length === 0) return;

    let activeLink = null;
    let ticking = false;

    function pageTop(el) {
      return el.getBoundingClientRect().top + window.scrollY;
    }

    function updateActive() {
      const threshold = window.scrollY + window.innerHeight * 0.28;
      let chosen = targets[0];
      for (let i = targets.length - 1; i >= 0; i--) {
        if (threshold >= pageTop(targets[i])) {
          chosen = targets[i];
          break;
        }
      }
      const nextLink = byId.get(chosen.id) || null;
      if (nextLink !== activeLink) {
        if (activeLink) activeLink.classList.remove("is-active");
        if (nextLink) nextLink.classList.add("is-active");
        activeLink = nextLink;
      }
      ticking = false;
    }

    window.addEventListener(
      "scroll",
      () => {
        if (!ticking) {
          window.requestAnimationFrame(updateActive);
          ticking = true;
        }
      },
      { passive: true }
    );
    updateActive();
  }

  function initSidenotes() {
    const grid = document.querySelector(".pf-essay-grid");
    const prose = document.querySelector(".pf-essay-prose");
    const rail = document.querySelector(".pf-essay-sidenotes");
    if (!grid || !prose || !rail) return;

    const footnoteContainer =
      prose.querySelector(".footnotes, .footnotes-list, section[role='doc-endnotes']") ||
      document.querySelector(".footnotes, section[role='doc-endnotes']");

    const refs = Array.from(
      prose.querySelectorAll("a.footnote-ref[href^='#fn'], sup a[href^='#fn']")
    );
    if (refs.length === 0) return;

    const wideQuery = window.matchMedia("(min-width: 1181px)");

    function layout() {
      rail.innerHTML = "";
      if (!wideQuery.matches) {
        if (footnoteContainer) footnoteContainer.classList.remove("pf-footnotes-enhanced");
        return;
      }

      const gridTop = grid.getBoundingClientRect().top + window.scrollY;
      const placed = [];

      refs.forEach((ref, idx) => {
        const targetId = decodeURIComponent((ref.getAttribute("href") || "").slice(1));
        const fnItem = document.getElementById(targetId);
        if (!fnItem) return;

        const note = document.createElement("div");
        note.className = "pf-sidenote";
        note.innerHTML = fnItem.innerHTML;
        note.querySelectorAll(".footnote-backref, a[role='doc-backlink']").forEach((el) => el.remove());

        const num = document.createElement("span");
        num.className = "pf-sidenote-num";
        num.textContent = String(idx + 1);
        note.prepend(num);

        rail.appendChild(note);

        const anchorY = ref.getBoundingClientRect().top + window.scrollY - gridTop - 2;
        const prev = placed[placed.length - 1];
        const top = prev ? Math.max(anchorY, prev.top + prev.height + 16) : Math.max(0, anchorY);
        note.style.top = `${Math.round(top)}px`;
        placed.push({ top, height: note.offsetHeight });

        const hi = (on) => note.classList.toggle("is-highlighted", on);
        ref.addEventListener("mouseenter", () => hi(true));
        ref.addEventListener("mouseleave", () => hi(false));
        ref.addEventListener("focus", () => hi(true));
        ref.addEventListener("blur", () => hi(false));
      });

      const tail = placed.length ? placed[placed.length - 1].top + placed[placed.length - 1].height : 0;
      rail.style.minHeight = `${Math.max(prose.offsetHeight, tail)}px`;
      if (footnoteContainer) footnoteContainer.classList.add("pf-footnotes-enhanced");
    }

    layout();
    window.addEventListener("resize", layout);
    window.addEventListener("load", layout);
    if (document.fonts && document.fonts.ready) {
      document.fonts.ready.then(layout).catch(() => {});
    }
  }

  function boot() {
    initThemeControls();
    initTocAndScrollSpy();
    initSidenotes();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    boot();
  }
})();
