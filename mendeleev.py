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


def divide(tail):
    result = []

    start = 0
    length = len(ELEMENTS)
    shift = 0

    while shift < len(tail):
        start, length = search(start, length, shift, tail[shift])
        if length == 0:
            break

        shift += 1
        if len(ELEMENTS[start]) == shift:
            result.append((ELEMENTS[start], tail[shift:]))
            start += 1
            length -= 1

    return result or [("?", tail[1:])]


def advance(els, tail):
    return [(els + [e], t) for (e, t) in divide(tail)]


def explode(word):
    result = [([], word.lower().encode())]
    while True:
        new = []
        tail = None
        for res in result:
            if res[1]:
                adv = advance(*res)
                new.extend(adv)
                if not tail:
                    tail = adv[0][1]
            else:
                new.append(res)

        result = new

        if not tail:
            break

    return [els for els, _ in result]


for w in sys.argv[1:]:
    print(w + ":")
    for f in filter(None, explode(w)):
        print(" " + " ".join(f))
