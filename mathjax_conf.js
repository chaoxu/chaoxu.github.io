MathJax.Hub.Register.StartupHook("HTML-CSS Jax Ready",function () {
  var HTMLCSS = MathJax.OutputJax["HTML-CSS"],
      MBASE = MathJax.ElementJax.mml.mbase.prototype;
  HTMLCSS.Augment({
    initHTML: function (math,span) {
      this.em = MBASE.em = this.em/this.scale;
      this.linebreakwidth *= this.scale;
      this.scale = 1; span.style.fontSize = "100%";
    }
  });
});
MathJax.Hub.Config({
  showMathMenu: false,
  messageStyle: "none",
    tex2jax: {
      processEscapes: true,
      ignoreClass: "tex2jax_ignore",
      processClass: "math"//,
      //inlineMath: [['$','$'], ['\\(','\\)']],
      //displayMath: [['$$','$$'], ['\\[','\\]']]
    },
  "HTML-CSS": {
    availableFonts: ["STIX","TeX"],
    preferredFont: "TeX",
    webFont: "TeX"
  },
  TeX: { 
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
MathJax.Ajax.loadComplete("http://www.chaoxuprime.com/mathjax_conf.js");