---
title: README
---

# Technology

1.  The source of the blog is on a [git repository](https://github.com/chaoxu/chaoxu.github.io/tree/develop). 
2.  html files are hosted on [GitHub Pages](https://pages.github.com). 
3.  The blog source is compiled by [hakyll](http://jaspervdj.be/hakyll/).
4.  The content is written use my variation of [Pandoc's Markdown](http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown), I call it [ChaoDoc](https://github.com/chaoxu/chaoxu.github.io/blob/develop/ChaoDoc.hs).
5.  Coded most of the things in [Sublime Text 3](http://www.sublimetext.com/3).
9.  The theme is [Tarski](http://tarskitheme.com/) with few changes.
10. The math are shown with [KaTex](https://katex.org/), rendered offline.
11. The heading themes are similar to [Connections in Combinatorial Optimization](http://www.amazon.com/gp/product/0199205272/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0199205272&linkCode=as2&tag=fighterempire-20), and the remaining themes are similar to [Categories for the Working Mathematician](http://www.amazon.com/gp/product/0387984038/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0387984038&linkCode=as2&tag=fighterempire-20). The symbols are of KaTeX.
12. To compile just `ChaoDoc.hs`, you need `pandoc` and `citeproc`. To compile the `site.hs`, additionally you need `hakyll`.
13. For the homepage, I generate with python. Needs to run `pip install pyyaml mistune jinja2 bs4` on a new machine. 
14. KaTeX offline compilation. Requires [katex_cli](https://github.com/chaoxu/katex_cli). Build it and copy the executable into the base directory, and name it `katex_cli`.

# Notes

- I made an [syntax highlighting file](https://gist.github.com/chaoxu/195ce33124f384a2f4e4) for sublime 3, so in Markdown it can also highlight latex codes.
