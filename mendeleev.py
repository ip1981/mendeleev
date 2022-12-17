import sys


# fmt: off
ELEMENTS = [
  "Ac", "Ag", "Al", "Am", "Ar", "As", "At", "Au", "B", "Ba", "Be", "Bh",
  "Bi", "Bk", "Br", "C", "Ca", "Cd", "Ce", "Cf", "Cl", "Cm", "Cn", "Co",
  "Cr", "Cs", "Cu", "Db", "Ds", "Dy", "Er", "Es", "Eu", "F", "Fe", "Fl",
  "Fm", "Fr", "Ga", "Gd", "Ge", "H", "He", "Hf", "Hg", "Ho", "Hs", "I",
  "In", "Ir", "K", "Kr", "La", "Li", "Lr", "Lu", "Lv", "Mc", "Md", "Mg",
  "Mn", "Mo", "Mt", "N", "Na", "Nb", "Nd", "Ne", "Nh", "Ni", "No", "Np",
  "O", "Og", "Os", "P", "Pa", "Pb", "Pd", "Pm", "Po", "Pr", "Pt", "Pu",
  "Ra", "Rb", "Re", "Rf", "Rg", "Rh", "Rn", "Ru", "S", "Sb", "Sc", "Se",
  "Sg", "Si", "Sm", "Sn", "Sr", "Ta", "Tb", "Tc", "Te", "Th", "Ti", "Tl",
  "Tm", "Ts", "U", "V", "W", "Xe", "Y", "Yb", "Zn", "Zr"
]
# fmt: on

elements = [el.lower().encode() for el in ELEMENTS]


def search(start, length, shift, char):
    upper = start + length
    lower = start
    while lower < upper:
        mid = int((lower + upper) / 2)
        if elements[mid][shift] < char:
            lower = mid + 1
        else:
            upper = mid

    if lower == start + length:
        return (0, 0)

    if elements[lower][shift] != char:
        return (0, 0)

    upper = start + length
    start = lower
    while lower < upper:
        mid = int((lower + upper) / 2)
        if char < elements[mid][shift]:
            upper = mid
        else:
            lower = mid + 1

    length = upper - start

    return (start, length)


def split(tail):
    result = []

    start = 0
    length = len(ELEMENTS)
    shift = 0

    while shift < len(tail):
        start, length = search(start, length, shift, tail[shift])
        if length == 0:
            break

        shift += 1
        if len(elements[start]) == shift:
            result.append((ELEMENTS[start], tail[shift:]))
            start += 1
            length -= 1

    return result or [("?", tail[1:])]


def explode(tail):
    return [(x[0], explode(x[1]) if x[1] else None) for x in split(tail)]


def analyze(word):
    return explode(word.lower().encode())


def print_plain(tree, formula):
    for x in tree:
        formula.append(x[0])
        if x[1]:
            print_plain(x[1], formula)
        else:
            print(" " + " ".join(formula))
        formula.pop()


for w in sys.argv[1:]:
    print(w + ":")
    if w:
        print_plain(analyze(w), [])
