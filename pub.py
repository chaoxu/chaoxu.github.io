import yaml
import re
import mistune

mk = mistune.Markdown(parse_block_html=True)

def mki(x): 
    return mk(x)[3:-5]

def coauthor_list(xs, links):
    xs = [x for x in xs if x!="Chao Xu"]
    if not xs:
        return ""
    out = '<ul class="coauthor-list">'
    for x in xs:
        s = x
        if x in links.keys():
            if links[x]:
                s = '<a href="'+links[x]+'">'+x+'</a>'
        out+= "<li>"+s+"</li>"
    return out+"</ul>"

def yaml_loader(filepath):
    """Load a yaml file."""
    f = open(filepath, "r")
    s = f.read()
    data = yaml.load_all(s)
    return data

def gen_left(show,idd):
    if 'a' in show:
        a = '<label class="showmore material-icons" for="'+idd+'-toggle"></label>'
    else:
        a = '<i class="material-icons disabled">add</i>'

    if 'd' in show:
        d = '<a href="files/papers/'+idd+'.pdf"><i class="material-icons">description</i></a>'
    else:
        d = '<i class="material-icons disabled">description</i>'
    if 'p' in show:
        p = '<a href="files/presentations/'+idd+'.pdf"><i class="material-icons">airplay</i></a>'
    else:
        p = '<i class="material-icons disabled">airplay</i>'
    return a+d+p

def math(string):
    # find $ $ pairs
    return re.sub(r'\$(.*?)\$', r'<span class="math">\1</span>', string)

def bib(pub, year, bib, venues):
    z = bib
    if not z:
        if pub:
            z = '<abbr title="'+venues[pub]+'">'+pub+'</abbr>' + ' <time>'+str(year)+'</time>.'
        else:
            z = '<time>'+str(year)+'</time>.'
    return '<p><em>'+z+'</em></p>'

def notes(ns):
    out = ""
    if not ns:
        return out
    for x in ns:
        out+='<aside class="note">'+mki(math(x))+'</aside>'
    return out

def build_paper(paper):

    if "show" not in paper.keys():
        paper["show"] = []

    idd = paper["id"]
    idtoggle = idd+"-toggle"
    if "abstract" in paper.keys():
        paper["show"].append("a")

    if "bib" not in paper.keys():
        paper["bib"] = ""
    if "pub" not in paper.keys():
        paper["pub"] = ""

    paper["title"] = math(paper["title"])

    left = '<div class="cv-left">'+gen_left(paper["show"],idd)+'</div>'

    title = '<cite class="paper-title">'+paper["title"]+'</cite>'

    toggle = ""
    abstract =""
    if 'a' in paper["show"]:
        abstract = '<div class="abstract">'+mk(math(paper["abstract"]))
        if "dedication" in paper.keys():
            abstract+='<aside class="dedication">'+mki(paper["dedication"])+'</aside>' 
        abstract +='</div>'
        toggle = '<input type="checkbox" class="abstract-guard" id="'+idtoggle+'">'

    if "notes" in paper.keys():
        notess = notes(paper["notes"])
    else:
        notess = ""

    right = ('<div class="cv-right">'+
             title+
             coauthor_list(paper["authors"],people)+
             bib(paper["pub"],paper["year"],paper["bib"],venues)+
             notess+
             '</div>')
    
    out = ('<!-- ' +paper["title"] +' -->\n'+
          '<div class="row cv-entry" id="'+idd+'">'+
          toggle+
          left+right+abstract+'</div>\n')
    return out

def build_papers(papers):
    return ''.join(map(build_paper,papers))


t = list(yaml_loader("pub.yaml"))
venues = t[0]["venues"]
people = t[0]["people"]
t = t[1:]

out = ''
out+='<div class="row pubtypeheader">Conference Publications</div>'
out+=build_papers([paper for paper in t if paper["type"]=="conference"])

out+= '<div class="row pubtypeheader">Journal Publications</div>'
out+= build_papers([paper for paper in t if paper["type"]=="journal"])

out+= '<div class="row pubtypeheader"><span>Manuscripts<br /><span style="font-size:0.6em"><em>Some manuscripts are available upon request.</em></span></span></div>'
out+= build_papers([paper for paper in t if paper["type"]=="manuscript"])

out+= '<div class="row pubtypeheader">Thesis</div>'
out+= build_papers([paper for paper in t if paper["type"]=="thesis"])

print(out)
