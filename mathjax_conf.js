MathJax.Hub.Config({
  showMathMenu: false,
  jax: ["input/TeX","output/CommonHTML"],
  extensions: ["tex2jax.js"],
  messageStyle: "none",
    tex2jax: {
      processEscapes: true,
      ignoreClass: "tex2jax_ignore",
      processClass: "math"//,
      //inlineMath: [['$','$'], ['\\(','\\)']],
      //displayMath: [['$$','$$'], ['\\[','\\]']]
    },
  //"HTML-CSS": {
  //  availableFonts: ["STIX","TeX"],
  //  preferredFont: "STIX",
  //  webFont: "STIX"
  //},
  TeX: { 
    //extensions: ["AMSmath.js","AMSsymbols.js"],
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
     } 
    }
});
MathJax.Ajax.loadComplete("http://chaoxuprime.com/mathjax_conf.js");