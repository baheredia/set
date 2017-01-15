l = [[i,j,k,l] for i in [1,2,3] for j in [1,2,3] for k in [1,2,3] for l in [1,2,3]]

def nombre(p):
    return "c" + "{d[0]}{d[1]}{d[2]}{d[3]}".format(d=p) + ".tex"

def dentro(p):
    w = """\\documentclass{article}
\\usepackage{setdeck}
\\begin{document}
\\pagestyle{empty}\n"""
    ww = "\\setcard{" + "{}".format(p[0]) + "}{" + "{}".format(p[1]) + "}{" + "{}".format(p[2]) + "}{" + "{}".format(p[3]) + "}{1}\n"
    return w+ww+"\\end{document}\n"

lwords = [(nombre(p),dentro(p)) for p in l]

for (nom, den) in lwords:
    fil = open(nom, 'a')
    fil.write(den)
    fil.close()
