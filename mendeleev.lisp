(defconstant +elements+
   #("?"
     "Ac" "Ag" "Al" "Am" "Ar" "As" "At" "Au" "B"  "Ba" "Be" "Bh"
     "Bi" "Bk" "Br" "C"  "Ca" "Cd" "Ce" "Cf" "Cl" "Cm" "Cn" "Co"
     "Cr" "Cs" "Cu" "Db" "Ds" "Dy" "Er" "Es" "Eu" "F"  "Fe" "Fl"
     "Fm" "Fr" "Ga" "Gd" "Ge" "H"  "He" "Hf" "Hg" "Ho" "Hs" "I"
     "In" "Ir" "K"  "Kr" "La" "Li" "Lr" "Lu" "Lv" "Mc" "Md" "Mg"
     "Mn" "Mo" "Mt" "N"  "Na" "Nb" "Nd" "Ne" "Nh" "Ni" "No" "Np"
     "O"  "Og" "Os" "P"  "Pa" "Pb" "Pd" "Pm" "Po" "Pr" "Pt" "Pu"
     "Ra" "Rb" "Re" "Rf" "Rg" "Rh" "Rn" "Ru" "S"  "Sb" "Sc" "Se"
     "Sg" "Si" "Sm" "Sn" "Sr" "Ta" "Tb" "Tc" "Te" "Th" "Ti" "Tl"
     "Tm" "Ts" "U"  "V"  "W"  "Xe" "Y"  "Yb" "Zn" "Zr"))

(defconstant +nelems+
  (1- (length +elements+)))

(defconstant +elems+
  (map 'vector #'string-downcase +elements+))

(defun get-part (el sh)
  (schar (elt +elems+ el) sh))

(defun search-el (range sh ch)
  (prog* ((top (+ (car range) (cdr range)))
          (u top)
          (l (car range))
          m)
        (loop while (< l u) do
              (setf m (truncate (+ l u) 2))
              (if (char< (get-part m sh) ch)
                (setf l (1+ m))
                (setf u m)))
        (when (or (= l top) (char/= ch (get-part l sh)))
          (setf (cdr range) 0) (return))
        (setf u top
              (car range) l)
        (loop while (< l u) do
              (setf m (truncate (+ l u) 2))
              (if (char< ch (get-part m sh))
                (setf u m)
                (setf l (1+ m))))
        (setf (cdr range) (- u (car range))) (return)))

(defun divide (tail)
  (prog ((range (cons 1 +nelems+))
         (sh 0)
         (r ()))
        (loop
          (search-el range sh (schar tail sh))
          (when (= 0 (cdr range)) (return))
          (incf sh)
          (when (= sh (length (elt +elems+ (car range))))
            (push (cons (car range) (subseq tail sh)) r)
            (incf (car range)) (decf (cdr range)))
          (when (= sh (length tail)) (return)))
        (when (null r) (push (cons 0 (subseq tail 1)) r))
        (return (reverse r))))

(defun explode (tail)
  (let ((sibs (divide tail))
        (next (lambda (x)
                (cons (car x)
                      (when (string/= "" (cdr x))
                        (explode (cdr x)))))))
    (map-into sibs next sibs)))

(defun print-plain (tree formula)
  (loop for x in tree do
        (vector-push (car x) formula)
        (if (cdr x)
          (print-plain (cdr x) formula)
          (progn
            (loop for i across formula do
                  (format t " ~a" (elt +elements+ i)))
            (fresh-line)))
        (decf (fill-pointer formula))))


(defun analyze (word)
  (let ((formula (make-array (length word) :fill-pointer 0))
        (tail (string-downcase word)))
    (format t "~a:" word) (fresh-line)
    (when (string/= "" tail)
      (print-plain (explode tail) formula))))

(defun program-args ()
  (or
   #+CLISP *args*
   #+ECL   (cdr ext:*unprocessed-ecl-command-args*)
   #+GCL   (cdr si::*command-args*)
   #+SBCL  (cdr *posix-argv*)
   nil))

(loop for word in (program-args) do
      (analyze word))

