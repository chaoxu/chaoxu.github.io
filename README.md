---
title: README
---

I chose to make this blog because I want to use [Haskell](http://www.haskell.org/) and save some hosting money by not hosting a [Drupal](http://drupal.org/) on [A2 Hosting](http://www.a2hosting.com/).

# Technology

1.  The source of the blog is on a [git repository](https://github.com/Mgccl/blog). 
2.  html files are hosted on [site44](http://www.site44.com/). 
3.  Using site44's rewrite system to direct all `/files` request to [amazon s3](http://aws.amazon.com/s3/), see [`/redirects.site44.txt`](/redirects.site44.txt). Also the mimetypes texts are useful. See [`/mimetypes.site44.txt`](/mimetypes.site44.txt)
4.  The blog source is compiled by [hakyll](http://jaspervdj.be/hakyll/).
5.  The content is written use my variation of [Pandoc's Markdown](http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown), I call it [Xu's MathDoc](https://github.com/Mgccl/blog/blob/master/MathDoc.hs).
6.  A simple rsync to sync compiled data to the blog. Something like `rsync -r ~/blog/_site/ ~/Dropbox/Apps/site44/www.chaoxuprime.com/ --checksum`
7.  site44 doesn't support top level domains. Use [wwwizer](http://wwwizer.com/naked-domain-redirect) to emulate the effect.
8.  Coded most of the things in [Sublime Text 2](http://www.sublimetext.com/2).
9.  The theme is [Tarski](http://tarskitheme.com/) with few changes.
10. The math are shown with [MathJax](http://www.mathjax.org).
11. The heading themes are similar to [Connections in Combinatorial Optimization](http://www.amazon.com/gp/product/0199205272/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0199205272&linkCode=as2&tag=fighterempire-20), and the remaining themes are similar to [Categories for the Working Mathematician](http://www.amazon.com/gp/product/0387984038/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=0387984038&linkCode=as2&tag=fighterempire-20). The symbols are of [STIX fonts](http://www.stixfonts.org/), or mathjax's own.
12. Uses [Detect Whether a Font is Installed](http://www.kirupa.com/html5/detect_whether_font_is_installed.htm) to fall back on mathjax webfont if STIX is not installed.
13. [Hyphenator.js](https://code.google.com/p/hyphenator/) to make the it look even more latex like.
14. To compile just `MathDoc.hs`, you need `pandoc` and `pandoc-citeproc`. To compile the `site.hs`, additionally you need `hakyll`(obviously...).

# Todo

- Able to simulate `\label` and `\ref`.

# Notes

- Under mac, MathDoc require you to `export LANG=C` for it to work, I have no idea why.
- Remember to set cache to amazon s3 . Say, set `Cache-Control` to `public, max-age=31536000`
- I made an [syntax highlighting file](https://gist.github.com/chaoxu/195ce33124f384a2f4e4) for sublime 3, so in Markdown it can also highlight latex codes.
