use std::{collections::LinkedList, env, ops::Range, str};

#[rustfmt::skip]
static ELEMENTS : [&[u8]; 119] = [ b"?",
  b"Ac", b"Ag", b"Al", b"Am", b"Ar", b"As", b"At", b"Au", b"B",  b"Ba", b"Be", b"Bh",
  b"Bi", b"Bk", b"Br", b"C",  b"Ca", b"Cd", b"Ce", b"Cf", b"Cl", b"Cm", b"Cn", b"Co",
  b"Cr", b"Cs", b"Cu", b"Db", b"Ds", b"Dy", b"Er", b"Es", b"Eu", b"F",  b"Fe", b"Fl",
  b"Fm", b"Fr", b"Ga", b"Gd", b"Ge", b"H",  b"He", b"Hf", b"Hg", b"Ho", b"Hs", b"I",
  b"In", b"Ir", b"K",  b"Kr", b"La", b"Li", b"Lr", b"Lu", b"Lv", b"Mc", b"Md", b"Mg",
  b"Mn", b"Mo", b"Mt", b"N",  b"Na", b"Nb", b"Nd", b"Ne", b"Nh", b"Ni", b"No", b"Np",
  b"O",  b"Og", b"Os", b"P",  b"Pa", b"Pb", b"Pd", b"Pm", b"Po", b"Pr", b"Pt", b"Pu",
  b"Ra", b"Rb", b"Re", b"Rf", b"Rg", b"Rh", b"Rn", b"Ru", b"S",  b"Sb", b"Sc", b"Se",
  b"Sg", b"Si", b"Sm", b"Sn", b"Sr", b"Ta", b"Tb", b"Tc", b"Te", b"Th", b"Ti", b"Tl",
  b"Tm", b"Ts", b"U",  b"V",  b"W",  b"Xe", b"Y",  b"Yb", b"Zn", b"Zr"
];

struct Split<'a> {
    eid: usize,
    tail: &'a [u8],
}

struct Element {
    eid: usize,
    next: Option<LinkedList<Element>>,
}

fn search(range: &mut Range<usize>, shift: usize, mut c: u8) {
    let mut u = range.end;
    let mut l = range.start;
    c |= b' ';

    while l < u {
        let m = (l + u) / 2;
        if (ELEMENTS[m][shift] | b' ') < c {
            l = m + 1;
        } else {
            u = m;
        }
    }

    if !range.contains(&l) || ((ELEMENTS[l][shift] | b' ') != c) {
        range.end = 0;
        return;
    }

    u = range.end;
    range.start = l;
    while l < u {
        let m = (l + u) / 2;
        if c < (ELEMENTS[m][shift] | b' ') {
            u = m;
        } else {
            l = m + 1;
        }
    }

    range.end = u;
}

fn split(tail: &[u8]) -> LinkedList<Split> {
    let mut x = LinkedList::new();
    let mut shift = 0;
    let mut range = 1..ELEMENTS.len() - 1;

    while shift < tail.len() {
        search(&mut range, shift, tail[shift]);
        if range.is_empty() {
            break;
        }
        shift += 1;
        if shift == ELEMENTS[range.start].len() {
            x.push_back(Split {
                eid: range.start,
                tail: &tail[shift..],
            });
            range.start += 1;
        }
    }

    if x.is_empty() {
        x.push_back(Split {
            eid: 0,
            tail: &tail[1..],
        });
    }

    x
}

fn explode(tail: &[u8]) -> LinkedList<Element> {
    split(tail)
        .into_iter()
        .map(|x| Element {
            eid: x.eid,
            next: if x.tail.is_empty() {
                None
            } else {
                Some(explode(x.tail))
            },
        })
        .collect()
}

fn print_plain(tree: &LinkedList<Element>, formula: &mut Vec<usize>) {
    for x in tree {
        formula.push(x.eid);
        if let Some(next) = &x.next {
            print_plain(next, formula);
        } else {
            for i in formula.iter() {
                print!(" {}", unsafe { str::from_utf8_unchecked(ELEMENTS[*i]) });
            }
            println!();
        }
        formula.pop();
    }
}

fn main() {
    for word in env::args().skip(1) {
        println!("{word}:");
        if !word.is_empty() {
            let tail = word.as_bytes();
            print_plain(&explode(tail), &mut Vec::with_capacity(tail.len()));
        }
    }
}
