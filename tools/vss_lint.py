"""Structural lint for a VolleyStation .vss workbook.

Checks structure, never meaning: there is no VQL evaluator outside
VolleyStation Pro, so a file that lints clean can still return the wrong
number. Report it as written and linted, never as correct.
"""
import json, re, sys, collections

# documented VQL surface
FUNCS = {"VSCOUNT","VSPER","VSEFF","VSCOURT","VSCELL","VSAND","VSOR","VSPLAYS",
         "VSRANGE","VSMAP","VSLINEUP","VSFORMAT","VSPLAYERNAME","VSTEAMNAME",
         "VSPLAYER","VSSET","VSPOINT","VSSCORE"}
FILTERS = {"Z","S","B","P","R","PA","PR","PC","PO","PS","PZ","M","NOT","W","L"}
KEEP = ("conditions","styles","formats","spans","sizes","data","table")

def cells(sheet):
    for c in sheet.get("content",{}).get("data",[]) or []:
        if isinstance(c,list) and len(c)>=3:
            yield c

def lint(path):
    raw = open(path, encoding="utf-8").read()
    try:
        doc = json.loads(raw)
    except Exception as e:
        return [("FATAL","file","not valid JSON: %s" % e)], None
    out = []
    if not isinstance(doc, list):
        out.append(("FATAL","file","top level is %s, expected a list of sheets" % type(doc).__name__))
        return out, doc
    for si, sh in enumerate(doc):
        nm = sh.get("name", "<unnamed %d>" % si)
        content = sh.get("content")
        if not isinstance(content, dict):
            out.append(("FATAL", nm, "no content block")); continue
        for k in KEEP:
            if k not in content:
                out.append(("SUBSTANTIVE", nm, "missing '%s' key; a rewrite that drops it loses %s" %
                            (k, {"conditions":"conditional formatting","spans":"merged cells",
                                 "sizes":"row/column sizing"}.get(k,"state"))))
        styles = content.get("styles") or []
        nstyle = len(styles)
        seen_style = set()
        for c in cells(sh):
            r, col, val = c[0], c[1], c[2]
            if not isinstance(r,int) or not isinstance(col,int) or r<1 or col<1:
                out.append(("FATAL", nm, "cell with bad address %r" % (c[:2],)))
            if len(c) >= 4 and isinstance(c[3], str):
                seen_style.add(c[3])
            if not isinstance(val, str) or not val.startswith("="):
                continue
            f = val
            if f.count("(") != f.count(")"):
                out.append(("FATAL", nm, "R%dC%d unbalanced parentheses: %s" % (r,col,f[:90])))
            if f.count('"') % 2:
                out.append(("FATAL", nm, "R%dC%d unbalanced quotes: %s" % (r,col,f[:90])))
            for fn in set(re.findall(r"\b(VS[A-Z]+)\s*\(", f)):
                if fn not in FUNCS:
                    out.append(("SUBSTANTIVE", nm, "R%dC%d unknown function %s" % (r,col,fn)))
            # filter tokens inside quoted selectors, after a ';'
            for sel in re.findall(r'"([^"]*)"', f):
                if ";" not in sel: continue
                for part in sel.split(";")[1:]:
                    tok = re.match(r"[A-Z]+", part.strip())
                    if tok and tok.group(0) not in FILTERS:
                        out.append(("SUBSTANTIVE", nm,
                                    "R%dC%d filter token '%s' not in the documented set (%s)"
                                    % (r,col,tok.group(0),sel[:40])))
            # VSCELL placeholder arity: "(n)" in the selector must match args
            for m in re.finditer(r'VSCELL\(\s*"([^"]*)"\s*((?:,[^()]*|,[^,]*\([^()]*\))*)\)', f):
                sel, args = m.group(1), m.group(2)
                want = len(set(re.findall(r"\((\d+)\)", sel)))
                have = len([a for a in args.split(",") if a.strip()])
                if want != have:
                    out.append(("FATAL", nm, "R%dC%d VSCELL wants %d argument(s) for %s but got %d"
                                % (r,col,want,sel[:30],have)))
        unknown = seen_style - {s[0] for s in styles if isinstance(s,list) and s} - set()
        if styles and isinstance(styles[0], list):
            known = {s[0] for s in styles if s}
            bad = seen_style - known
            if bad:
                out.append(("SUBSTANTIVE", nm, "%d cells reference styles not defined in the sheet: %s"
                            % (len(bad), ", ".join(sorted(bad)[:6]))))
    return out, doc

if __name__ == "__main__":
    issues, doc = lint(sys.argv[1])
    if doc:
        print("sheets: %d" % len(doc))
        for sh in doc:
            d = list(cells(sh))
            fs = [c[2] for c in d if isinstance(c[2],str) and c[2].startswith("=")]
            print("  %-28s cells=%5d formulas=%5d" % (sh.get("name","?")[:28], len(d), len(fs)))
    sev = collections.Counter(i[0] for i in issues)
    print("\nissues: " + (", ".join("%s %d" % (k,v) for k,v in sev.items()) if sev else "none"))
    for lvl in ("FATAL","SUBSTANTIVE","COSMETIC"):
        for l,nm,msg in issues:
            if l==lvl: print("  [%s] %s: %s" % (l, nm, msg))
