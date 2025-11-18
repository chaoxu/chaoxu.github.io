// docmath.js
document.addEventListener('DOMContentLoaded', () => {
  const vars = Object.create(null);

  function parseNumber(text) {
    const v = parseFloat(String(text).trim());
    return Number.isNaN(v) ? 0 : v;
  }

function formatValue(value, fmt) {
  if (fmt === undefined) return String(value);

  if (fmt === "int")
    return Math.round(value).toString();

  if (fmt === "percent")
    return value.toFixed(2) + "%";

  if (fmt === "currency")
    return "$" + value.toFixed(2);

  // if fmt is a number like "0", "1", "2", etc.
  const n = parseInt(fmt, 10);
  if (!isNaN(n))
    return value.toFixed(n);

  return String(value);
}

function recompute() {
  const exprEls = document.querySelectorAll('.dm-expr');

  // include both variables and computed exprs
  const env = Object.assign({}, vars);

  exprEls.forEach(el => {
    const name = el.dataset.name;
    const expr = el.dataset.expr || "";
    const fmt = el.dataset.format;

    try {
      const fn = Function(...Object.keys(env), "return " + expr);
      const value = fn(...Object.values(env));

      if (typeof value === 'number' && Number.isFinite(value)) {
        el.textContent = formatValue(value, fmt);
      } else {
        el.textContent = String(value);
      }

      if (name) env[name] = value;

    } catch (e) {
      el.textContent = 'ERR';
    }
  });
}

  // Initialize variable elements
  const varEls = document.querySelectorAll('.dm-var');

  varEls.forEach(el => {
    const name = el.dataset.name;
    if (!name) return;

    // initial value from data-value or existing text
    const initial =
      el.dataset.value !== undefined
        ? el.dataset.value
        : el.textContent;

    const value = parseNumber(initial);
    vars[name] = value;

    // show the initial value
    el.textContent = value;

    // listen for user edits
    el.addEventListener('input', () => {
      const v = parseNumber(el.textContent);
      vars[name] = v;
      recompute();
    });
  });

  // initial compute
  recompute();
});