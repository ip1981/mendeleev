module Main
  ( main
  ) where

import Data.Char (toLower)
import System.Environment (getArgs)

elements :: [String]
elements = [
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

search :: Char -> [String] -> [String]
search c = takeWhile start . dropWhile (not . start)
  where
    start [] = False
    start (x:_) = toLower x == c'
    c' = toLower c

split :: String -> [(String, String)]
split [] = []
split (x:xs) =
  if null res
    then [("?", xs)]
    else res
  where
    res = go [] [] xs (search x elements)
    go r _ _ [] = r
    go r el rest candidates@(c:_) =
      let el' = head c : el
          candidates'@(c':_) = map tail candidates
          r' =
            if null c'
              then (reverse el', rest) : r
              else r
       in case rest of
            (y:ys) -> go r' el' ys (search y candidates')
            _ -> r'

advance :: ([String], String) -> [([String], String)]
advance (els, rest) = map collect $ split rest
  where
    collect (el, rest') = (el : els, rest')

explode :: String -> [[String]]
explode word = reverse . map (reverse . fst) $ go [([], word)]
  where
    go :: [([String], String)] -> [([String], String)]
    go [] = []
    go (x:xs) =
      if null (snd x)
        then x : go xs
        else go (advance x) ++ go xs

printFormula :: String -> IO ()
printFormula word = do
  putStr word
  putStrLn ":"
  mapM_ (putStrLn . (:) ' ' . unwords) . filter (not . null) $ explode word

main :: IO ()
main = getArgs >>= mapM_ printFormula
