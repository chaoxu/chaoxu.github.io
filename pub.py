import yaml
import re
import mistune
import html
import functools
import sys
from jinja2 import Template
import io
from bs4 import BeautifulSoup as bs
import cgi

def render_inline_math(renderer, text):
    return r'<span class="math">' + text + r'</span>'

def parse_inline_math(inline, m, state):
    text = m.group('math')
    state.append_token({'type': 'inline_math', 'raw': text})
    return m.end()

# parsing markdown with a modified built-in math plugin
INLINE_MATH_PATTERN = r'\$(?P<math>.+?)\$'
mk=mistune.create_markdown(escape=False)
mk.inline.register('inline_math', INLINE_MATH_PATTERN, parse_inline_math, before='link')
mk.renderer.register('inline_math', render_inline_math)

# def compose(*functions):
#     return functools.reduce(lambda f, g: lambda x: f(g(x)), functions, lambda x: x)

def mki(x): 
    return mk(x)[3:-5]

# def math(string):
#     def my_replace(match):
#         return '<span class="math">'+html.escape(match.group(1))+'</span>'

#     # find $ $ pairs
#     return re.sub(r'\$(.*?)\$', my_replace, string)

parse = mk
# parse=compose(mk,math)
# inline parse, strip the <p> tag
# parsei=compose(mki,math)
parsei = mki

def coauthor_list(xs, links):
    # xs = [x for x in xs if x!="Chao Xu"]
    y = []
    for x in xs:
        a = {}
        a["link"] = ""
        a["name"] = x
        if x in links.keys():
            a["link"] = links[x]
        y.append(a)
    return y

def yaml_loader(filepath):
    """Load a yaml file."""
    f = open(filepath, "r")
    s = f.read()
    data = yaml.load_all(s, Loader=yaml.FullLoader)
    return data

def build_paper(paper):

    if "show" not in paper.keys():
        paper["show"] = []

    #paper["title"] = math(paper["title"])
    paper["title"] = parsei(paper["title"])
    paper["authors"] = coauthor_list(paper["authors"],people)
    if "notes" in paper.keys():
        paper["notes"] = map(parsei,paper["notes"])
    if "pub" in paper.keys():
        a = {}
        a["name"] = paper["pub"] 
        a["venue"] = venues.get(paper["pub"],paper["pub"])
        # print paper["pub"]
        paper["pub"] = a
    if "abstract" in paper.keys():
        paper["show"].append("a")
        paper["abstract"] = parse(paper["abstract"])

    if "dedication" in paper.keys():
        paper["dedication"] = parsei(paper["dedication"])
    if "selected" in paper.keys():
        paper["selected"] = "cv-selected"
    else:
        paper["selected"] = "cv-non-selected"
    return paper

def build_papers(papers):
    return map(build_paper,papers)


t = list(yaml_loader("pub.yaml"))
venues = t[0]["venues"]
people = t[0]["people"]
types = t[0]["types"]
t = t[1:]

#types = {"conference": "Conference Publications", 
#         "journal":"Journal Publications", 
#         "manuscript":'<span>Manuscripts<br /><span style="font-size:0.6em"><em>Some manuscripts are available upon request.</em></span></span>', 
#         "thesis":"Thesis"}

parsed = []
for z in types.keys():
    q = {}
    q["title"] = types[z] 
    q["papers"] = build_papers([paper for paper in t if paper["type"]==z])
    parsed.append(q)

# print parsed

file = io.open("index_template.html", "r", encoding="utf-8") 
template = Template(file.read())
root = template.render(pub_types=parsed).splitlines()
filtered = list(filter(lambda x: not re.match(r'^\s*$', x), root))
#print("\n".join(filtered).encode('utf8'))
sys.stdout.buffer.write("\n".join(filtered).encode('utf8'))
#soup = bs(root)                #make BeautifulSoup
#prettyHTML = soup.prettify()
#print(prettyHTML.encode("utf-8"))