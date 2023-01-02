#include <cstddef>
#include <cstdlib>
#include <iostream>
#include <list>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

using std::list;
using std::optional;
using std::pair;
using std::string_view;
using std::vector;

static const vector<string_view> ELEMENTS = {
    "?",  "Ac", "Ag", "Al", "Am", "Ar", "As", "At", "Au", "B",  "Ba", "Be",
    "Bh", "Bi", "Bk", "Br", "C",  "Ca", "Cd", "Ce", "Cf", "Cl", "Cm", "Cn",
    "Co", "Cr", "Cs", "Cu", "Db", "Ds", "Dy", "Er", "Es", "Eu", "F",  "Fe",
    "Fl", "Fm", "Fr", "Ga", "Gd", "Ge", "H",  "He", "Hf", "Hg", "Ho", "Hs",
    "I",  "In", "Ir", "K",  "Kr", "La", "Li", "Lr", "Lu", "Lv", "Mc", "Md",
    "Mg", "Mn", "Mo", "Mt", "N",  "Na", "Nb", "Nd", "Ne", "Nh", "Ni", "No",
    "Np", "O",  "Og", "Os", "P",  "Pa", "Pb", "Pd", "Pm", "Po", "Pr", "Pt",
    "Pu", "Ra", "Rb", "Re", "Rf", "Rg", "Rh", "Rn", "Ru", "S",  "Sb", "Sc",
    "Se", "Sg", "Si", "Sm", "Sn", "Sr", "Ta", "Tb", "Tc", "Te", "Th", "Ti",
    "Tl", "Tm", "Ts", "U",  "V",  "W",  "Xe", "Y",  "Yb", "Zn", "Zr"};

typedef pair<size_t, string_view> Split;

struct Element {
  size_t eid;
  optional<list<Element>> next;
  Element(size_t eid_, optional<list<Element>> next_)
      : eid(eid_), next(next_) {}
};

static void search(size_t &start, size_t &end, size_t shift, char c) {
  size_t l, m, u;
  c |= ' ';

  u = end;
  l = start;
  while (l < u) {
    m = (l + u) / 2;
    if ((ELEMENTS[m][shift] | ' ') < c)
      l = m + 1;
    else
      u = m;
  }

  if ((l == end) || ((ELEMENTS[l][shift] | ' ') != c)) {
    end = 0;
    return;
  }

  u = end;
  start = l;
  while (l < u) {
    m = (l + u) / 2;
    if (c < (ELEMENTS[m][shift] | ' '))
      u = m;
    else
      l = m + 1;
  }

  end = u;
}

static list<Split> split(string_view tail) {
  auto x = list<Split>();

  size_t start = 1;
  size_t end = ELEMENTS.size();
  size_t shift = 0;

  while (shift < tail.length()) {
    search(start, end, shift, tail[shift]);
    if (start >= end)
      break;

    shift++;
    if (shift == ELEMENTS[start].length()) {
      x.emplace_back(start, tail.substr(shift));
      start++;
    }
  }

  if (x.empty())
    x.emplace_back(0, tail.substr(1));

  return x;
}

static list<Element> explode(string_view tail) {
  auto x = list<Element>();
  for (auto const &s : split(tail)) {
    x.emplace_back(s.first, s.second.empty()
                                ? std::nullopt
                                : std::make_optional(explode(s.second)));
  }
  return x;
}

static void print_plain(list<Element> const &tree, vector<size_t> &formula) {
  for (auto const &x : tree) {
    formula.push_back(x.eid);
    if (x.next)
      print_plain(*x.next, formula);
    else {
      for (auto i : formula)
        std::cout << " " << ELEMENTS[i];
      std::cout << std::endl;
    }
    formula.pop_back();
  }
}

int main(int argc, const char *argv[]) {
  vector<size_t> formula;

  for (int i = 1; i < argc; i++) {
    string_view word = argv[i];
    std::cout << word << ":" << std::endl;
    if (auto len = word.length(); len) {
      formula.reserve(len);
      print_plain(explode(word), formula);
    }
  }

  return EXIT_SUCCESS;
}
