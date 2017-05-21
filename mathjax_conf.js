MathJax.Hub.Config({
  showMathMenu: false,
  jax: ["input/TeX","output/CommonHTML"],
  extensions: ["tex2jax.js"],
  messageStyle: "none",
    tex2jax: {
      processEscapes: true,
      ignoreClass: "tex2jax_ignore",
      processClass: "math"
    },
  TeX: { 
    extensions: ["AMSmath.js","AMSsymbols.js","noErrors.js","noUndefined.js","autobold.js"],
    Macros: { 
        R: '{\\mathbb{R}}', 
        N: '{\\mathbb{N}}', 
        Z: '{\\mathbb{Z}}', 
        C: '{\\mathbb{C}}', 
        F: '{\\mathbb{F}}', 
        argmin: '{\\mathop{\\operatorname*{arg\\,min}}}',
        argmax: '{\\mathop{\\operatorname*{arg\\,max}}}',
        mex: '{\\mathop{\\operatorname{mex}}}', 
        lcm: '{\\mathop{\\operatorname{lcm}}}',
        bigtriangleright: '{\\mathop{\\Large \\triangleright}}',
        bigtriangleleft: '{\\mathop{\\Large \\triangleleft}}',
        set: ['\\left\\{ #1 \\right\\}',1],
        floor: ['\\left\\lfloor #1 \\right\\rfloor',1],
        ceil:  ['\\left\\lceil #1 \\right\\rceil',1],
        abs:  ['\\left| #1 \\right|',1]
     } 
    }
});

MathJax.Ajax.loadComplete("http://chaoxuprime.com/mathjax_conf.js");