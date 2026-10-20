"""Cell-by-cell diff of two .vss workbooks. Raw diff on the JSON is unreadable."""
import json, sys, collections

def load(p):
    d = json.load(open(p, encoding="utf-8"))
    out = {}
    for sh in d:
        nm = sh.get("name","?")
        cells = {}
        for c in (sh.get("content",{}).get("data") or []):
            if isinstance(c,list) and len(c)>=3:
                cells[(c[0],c[1])] = c[2]
        out[nm] = (cells, sh.get("content",{}))
    return out

def col(n):
    s=""
    while n: n,r = divmod(n-1,26); s = chr(65+r)+s
    return s or "?"

a, b = load(sys.argv[1]), load(sys.argv[2])
print("A: %s\nB: %s\n" % (sys.argv[1].split("\\")[-1], sys.argv[2].split("\\")[-1]))
for nm in sorted(set(a)|set(b)):
    if nm not in a: print("sheet only in B: %s" % nm); continue
    if nm not in b: print("sheet only in A: %s" % nm); continue
    ca, cb = a[nm][0], b[nm][0]
    added   = sorted(set(cb)-set(ca))
    removed = sorted(set(ca)-set(cb))
    changed = sorted(k for k in set(ca)&set(cb) if ca[k]!=cb[k])
    print("%s: %d changed, %d added, %d removed (A %d cells, B %d cells)"
          % (nm, len(changed), len(added), len(removed), len(ca), len(cb)))
    for k in changed[:60]:
        print("  %s%d" % (col(k[1]), k[0]))
        print("     A: %s" % str(ca[k])[:150])
        print("     B: %s" % str(cb[k])[:150])
    if len(changed) > 60: print("  ... %d more changed" % (len(changed)-60))
    for k in added[:15]:   print("  + %s%d = %s" % (col(k[1]), k[0], str(cb[k])[:110]))
    if len(added) > 15: print("  ... %d more added" % (len(added)-15))
    for k in removed[:15]: print("  - %s%d = %s" % (col(k[1]), k[0], str(ca[k])[:110]))
    if len(removed) > 15: print("  ... %d more removed" % (len(removed)-15))
    for key in ("conditions","spans","sizes","styles"):
        la, lb = len(a[nm][1].get(key) or []), len(b[nm][1].get(key) or [])
        if la != lb: print("  %s: %d -> %d" % (key, la, lb))
