#include<stdio.h>
#include<stdlib.h>
#include<string.h>

static const char *const ELEMENTS[] = { "?",
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
};

static const size_t NELEMENTS = sizeof (ELEMENTS) / sizeof (const char *) - 1;

typedef struct element
{
  size_t n;
  const char *tail;
  struct element *sibs;
  struct element *next;
} element_t;

static void
search (size_t *start, size_t *len, size_t shift, char c)
{
  size_t l, m, u;
  c |= ' ';

  u = *start + *len;
  l = *start;
  while (l < u)
    {
      m = (l + u) / 2;
      if ((ELEMENTS[m][shift] | ' ') < c)
        l = m + 1;
      else
        u = m;
    }

  if ((l == *start + *len) || ((ELEMENTS[l][shift] | ' ') != c))
    {
      *len = 0;
      return;
    }

  u = *start + *len;
  *start = l;
  while (l < u)
    {
      m = (l + u) / 2;
      if (c < (ELEMENTS[m][shift] | ' '))
        u = m;
      else
        l = m + 1;
    }

  *len = u - *start;
}

static element_t *
split (const char *tail)
{
  element_t *head = NULL;
  element_t *last = NULL;

  size_t start = 1;
  size_t len = NELEMENTS;
  size_t shift = 0;

  while (tail[shift])
    {
      search (&start, &len, shift, tail[shift]);
      if (!len)
        break;

      shift++;
      if (!ELEMENTS[start][shift])
        {
          element_t *el = (element_t *) malloc (sizeof (element_t));
          if (!el)
            break;

          if (!head)
            head = el;

          if (last)
            last->sibs = el;

          last = el;
          last->n = start;
          last->tail = &tail[shift];
          start++;
          len--;
        }
    }

  if (!head)
    {
      head = (element_t *) malloc (sizeof (element_t));
      if (!head)
        return NULL;
      head->n = 0;
      head->tail = &tail[1];
      last = head;
    }

  last->sibs = NULL;

  return head;
}

static element_t *
explode (const char *word)
{
  element_t *root = split (word);
  for (element_t * el = root; el; el = el->sibs)
    el->next = *el->tail ? explode (el->tail) : NULL;
  return root;
}

static void
free_elements (element_t * el)
{
  while (el)
    {
      if (el->next)
        free_elements (el->next);
      element_t *sib = el->sibs;
      free (el);
      el = sib;
    }
}

static void
print_plain (const element_t * current, size_t *formula, size_t n)
{
  for (const element_t * el = current; el; el = el->sibs)
    {
      formula[n] = el->n;
      if (el->next)
        print_plain (el->next, formula, n + 1);
      else
        {
          for (size_t i = 0; i <= n; i++)
            printf (" %s", ELEMENTS[formula[i]]);
          printf ("\n");
        }
    }
}

int
main (int argc, const char *argv[])
{
  for (int i = 1; i < argc; i++)
    {
      const char *word = argv[i];
      printf ("%s:\n", word);

      size_t len = strlen (word);
      if (!len)
        continue;

      element_t *root = explode (word);
      if (!root)
        return EXIT_FAILURE;

      size_t *formula = (size_t *) malloc (len * sizeof (size_t));
      if (!formula)
        return EXIT_FAILURE;

      print_plain (root, formula, 0);
      free (formula);
      free_elements (root);
    }

  return EXIT_SUCCESS;
}
