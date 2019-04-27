var args = process.argv.slice(2);

const fs = require("fs");
const filename = args[0]
const data = fs.readFileSync(filename, 'utf8');

const jsdom = require("jsdom");
const { JSDOM } = jsdom;
//const dom = new JSDOM(data);
const dom = new JSDOM(data);
const { window } = dom.window;
global.document = window.document;

const katex = require('katex');
const macros={
              "\\C":"\\mathbb{C}",
              "\\F":"\\mathbb{F}",
              "\\e": "\\varepsilon",
              "\\eps": "\\varepsilon",
              "\\mex": "\\mathop{\\operatorname{mex}}",
              "\\lcm": "\\mathop{\\operatorname{lcm}}",
              "\\dist": "\\mathop{\\operatorname{dist}}",
              "\\bigtriangleright": "{\\mathop{\\Large \\triangleright}}",
              "\\bigtriangleleft": "{\\mathop{\\Large \\triangleleft}}",
              '\\set':'\\left\\{ #1 \\right\\}',
              '\\floor':'\\left\\lfloor #1 \\right\\rfloor',
              '\\ceil':'\\left\\lceil #1 \\right\\rceil',
              '\\abs':'\\left\\| #1 \\right\\|'
              };
const mathElements = document.getElementsByClassName("math");
for (var i = 0; i < mathElements.length; i++) {
          var texText = mathElements[i].firstChild;
          if (mathElements[i].tagName == "SPAN") {
           katex.render(texText.data, mathElements[i], 
            { displayMode: mathElements[i].classList.contains("display"), throwOnError: false, macros:macros}
          );
        }
};
fs.writeFile(filename, dom.serialize(), function(err, data) {
  if (err) console.log(err);
  console.log("KaTeX Compiled for " + filename);
});