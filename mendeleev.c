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

typedef struct formula
{
  const char *tail;
  size_t n;
  size_t *els;
  struct formula *next;
} formula_t;


static void
search (size_t *start, size_t *len, size_t shift, char c)
{
  size_t l, m, u;
  char c_ = c | ' ';

  u = *start + *len;
  l = *start;
  while (l < u)
    {
      m = (l + u) / 2;
      if ((ELEMENTS[m][shift] | ' ') < c_)
        l = m + 1;
      else
        u = m;
    }

  if ((l == *start + *len) || ((ELEMENTS[l][shift] | ' ') != c_))
    {
      *len = 0;
      return;
    }

  u = *start + *len;
  *start = l;
  while (l < u)
    {
      m = (l + u) / 2;
      if (c_ < (ELEMENTS[m][shift] | ' '))
        u = m;
      else
        l = m + 1;
    }

  *len = u - *start;
}

static formula_t *
new_formula (const char *tail, size_t n, const size_t *els)
{
  formula_t *f = (formula_t *) malloc (sizeof (formula_t));
  if (!f)
    return NULL;

  f->els = (size_t *) malloc (sizeof (*f->els) * (n + strlen (tail)));
  if (!f->els)
    {
      free (f);
      return NULL;
    }

  if (n > 0)
    (void) memcpy (f->els, els, n * sizeof (*f->els));

  f->n = n;
  f->tail = tail;
  f->next = NULL;

  return f;
}

static void
free_formula (formula_t * f)
{
  while (f)
    {
      formula_t *next = f->next;
      if (f->els)
        free (f->els);
      free (f);
      f = next;
    }
}

static void
advance (formula_t * f)
{
  const char *tail = f->tail;
  size_t n = f->n;

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
          if (n != f->n)
            {
              formula_t *g = new_formula (tail, n, f->els);
              if (!g)
                break;
              g->next = f->next;
              f->next = g;
              f = g;
            }

          f->els[f->n++] = start;
          f->tail = &tail[shift];
          start++;
          len--;
        }
    }

  if (n == f->n)
    {
      f->els[f->n++] = 0;
      f->tail += 1;
    }
}

static formula_t *
explode (const char *word)
{
  formula_t *formula = new_formula (word, 0, NULL);
  if (!formula)
    return NULL;

  while (*word)
    {
      word = NULL;
      formula_t *f = formula;
      while (f)
        {
          if (*f->tail)
            {
              advance (f);
              if (!word)
                word = f->tail;
            }
          f = f->next;
        }
    }

  return formula;
}

int
main (int argc, const char *argv[])
{
  for (int w = 1; w < argc; w++)
    {
      const char *word = argv[w];
      printf ("%s:\n", word);

      formula_t *formula = explode (word);
      if (!formula)
        return EXIT_FAILURE;

      for (formula_t * f = formula; f; f = f->next)
        {
          for (size_t i = 0; i < f->n; i++)
            printf (" %s", ELEMENTS[f->els[i]]);
          if (f->n)
            printf ("\n");
        }
      free_formula (formula);
    }

  return EXIT_SUCCESS;
}
