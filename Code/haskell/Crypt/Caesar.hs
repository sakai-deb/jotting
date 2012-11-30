import qualified Data.Char as Char

-- Define Shift in lower character
chr2int :: Char -> Int
chr2int c = Char.ord c - Char.ord 'a'
int2chr :: Int -> Char
int2chr n = Char.chr (Char.ord 'a'  + n)
-- n-times shift one char in lower character
shift :: Int -> Char -> Char
shift n c
  | Char.isLower c =  int2chr $ (chr2int (Char.toLower c) + n) `mod` 26
  | otherwise = c

-- n-times shift one word
wordShift :: Int -> String -> String
wordShift n str = map (shift n) str
-- Define encodings
-- encode and decode string : Space is preserved by encode and decode.
encode :: Int -> String -> String
encode n xs = unwords $ map (wordShift n) (words xs)
decode n xs = encode (negate n) xs

-- Frequency analysis
isKey :: (Eq a) => a -> [(a,b)] -> Bool
isKey x xs = any (\(k,v) -> (x==k)) xs
freq :: String -> [(Char,Int)]
freq xs = foldr groupTapple [] xs
          where groupTapple x ys = if isKey x ys then map (\(x,v) -> (x,v+1)) ys else (x,1):ys
percent :: String -> [(Char,Float)]
percent xs = map (\(k,v) -> (k, 100 * fromIntegral(v) / fromIntegral(length(xs)) )) (freq xs)

-- Cracking Caesar
-- Statistical frequency
statPercent :: [(Char,Float)]
-- statPercent = [('a',8.2),('b',1.5),('c',2.8),('d',4.3),('e',12.7),('f',2.2),('g',2.0),('h',6.1),('i',7.0),('j', 0.2),('k',0.8),('l',4.0),('m',2.4),('n',6.7),('o',7.5),('p',1.9),('q',0.1),('r',6.0),('s',6.3),('t',9.1),('u',2.8),('v',1.0),('w',2.4),('x',0.2),('y',2.0),('z',0.1)]
statPercent = [('a',6.63),('b',1.2),('c',2.27),('d',3.45),('e',10.3),('f',1.92),('g',1.44),('h',4.82),('i',5.79),('j', 0.067),('k',0.55),('l',3.24),('m',1.99),('n',5.75),('o',6.01),('p',1.54),('q',0.09),('r',4.57),('s',5.4),('t',7.84),('u',2.47),('v',0.75),('w',1.92),('x',0.15),('y',1.27),('z',0.056)]

-- Chi-square test
evaluate :: [(Char,Float)] -> [(Char,Float)] -> Float
evaluate decrypt stat = sum [ ((c - s) ** 2.0) / s | (k,c) <- decrypt, (t,s) <- stat,k == t ]
-- Now that we can do "Just Cracking"
decrypt :: String -> String
decrypt crypt = decode (shift) crypt
                where
                  shift = head $ minPosition rotationDecryptTest
                  rotationDecryptTest = [ evaluate (percent (decode n crypt)) statPercent | n <- [0..length(statPercent)] ]
                  minPosition xs = [ n | (n,x) <- zip [0..length(statPercent)] xs, x == minimum xs]

-- Plain texts
string1 = map Char.toLower $ filter (`elem` (['a'..'z'] ++ ['A'..'Z'])) "London Bridge is broken down,\nBroken down, broken down.\nLondon Bridge is broken down,\nMy fair lady.\n\nBuild it up with wood and clay,\nWood and clay, wood and clay,\nBuild it up with wood and clay,\nMy fair lady.\n\nWood and clay will wash away,\nWash away, wash away,\nWood and clay will wash away,\nMy fair lady.\n\nBuild it up with bricks and mortar,\nBricks and mortar, bricks and mortar,\nBuild it up with bricks and mortar,\nMy fair lady.\n\nBricks and mortar will not stay,\nWill not stay, will not stay,\nBricks and mortar will not stay,\nMy fair lady.\n\nBuild it up with iron and steel,\nIron and steel, iron and steel,\nBuild it up with iron and steel,\nMy fair lady.\n\nIron and steel will bend and bow,\nBend and bow, bend and bow,\nIron and steel will bend and bow,\nMy fair lady.\n\nBuild it up with silver and gold,\nSilver and gold, silver and gold,\nBuild it up with silver and gold,\nMy fair lady.\n\nSilver and gold will be stolen away,\nStolen away, stolen away,\nSilver and gold will be stolen away,\nMy fair lady.\n\nSet a man to watch all night,\nWatch all night, watch all night,\nSet a man to watch all night,\nMy fair lady.\n\nSuppose the man should fall asleep,\nFall asleep, fall asleep,\nSuppose the man should fall asleep?\nMy fair lady.\n\nGive him a pipe to smoke all night,\nSmoke all night, smoke all night,\nGive him a pipe to smoke all night,\nMy fair lady"

string2 = map Char.toLower $ filter (`elem` (['a'..'z'] ++ ['A'..'Z'])) "\n\nIn the past,the ebuild environment has been defined by what Portage has supported. With the advent of alternative package managers,such a moving standard is no longer sufficient. The Package Manager Specification (PMS) aims to solve this by defining,independent of any package manager,what is and is not allowed in the tree,and what ebuilds may assume about their environment.\n\nIt is also required to document what each value of the EAPI ebuild variable actually means. At present PMS aims to document all Council - approved EAPIs.\n\nA git repository with the document's sources can be found at http://git.overlays.gentoo.org/gitweb/?p = proj/pms.git;a = summary. A convenient way to be up to date with the current document is the live ebuild found in the Gentoo repository,called app - doc/pms (TeX Live needs to be installed). Additionally the approved versions are available as ebuilds of that package,too and will install a normal PDF file only. "

test :: String -> Bool
test str = str == (decrypt str)
