module MathDoc ( mathdoc, mathdocInline) where
import Text.Pandoc
import Text.Regex (mkRegex, matchRegex)
import Data.Maybe
import Text.Pandoc.Writers.HTML
--import Data.String.Utils
import Data.Set (insert)
import System.Environment (getArgs)
import Data.List (nub, intercalate, isPrefixOf)
import Text.CSL.Pandoc
import System.IO.Unsafe
import Data.Array((!))
import Data.Bits((.|.))
import Data.Either

import qualified Data.Map as M

setMeta key val (Pandoc (Meta ms) bs) = Pandoc (Meta $ M.insert key val ms) bs

-- On mac, please do `export LANG=C` before using this thing
mathdocRead = def{readerExtensions = insert Ext_tex_math_double_backslash $ 
                                     insert Ext_tex_math_single_backslash $ 
                                     insert Ext_raw_tex pandocExtensions}
mathdocWrite = def{writerHTMLMathMethod = MathJax "",
                   writerHtml5          = True,
                   writerHighlight      = True,
                   writerNumberSections = True}

readDoc :: String -> Pandoc
readDoc x = head $ rights [readMarkdown mathdocRead x]

writeDoc :: Pandoc -> String
writeDoc x = writeHtmlString mathdocWrite (unsafePerformIO $ processCites' complete)
  where complete = setMeta "csl" (MetaInlines [Str "bib_style.csl"])
                   $ setMeta "link-citations" (MetaBool True)
                   $ setMeta "reference-section-title" (MetaInlines [Str "References"])
                   $ setMeta "bibliography" (MetaInlines [Str "reference.bib"]) x

writeDocT :: Pandoc -> String
writeDocT = writeHtmlString mathdocWrite

mathdoc :: String->String
mathdoc = compute . formatTheorem

mathdocInline :: String->String
mathdocInline = removeP . writeDocT . readDoc
  where removeP x = drop 3 (take ((length x) - 4) x) 
  
incrementBlock = ["Theorem",
                  "Conjecture",
                  "Definition",
                  "Example",
                  "Lemma",
                  "Problem",
                  "Proposition",
                  "Corollary"]
otherBlock = ["Proof","Remark"]

buildOr [x] = x
buildOr (l:ls) = l ++ '|' : buildOr ls
regex = "^\\{("
                 ++ buildOr (incrementBlock ++ otherBlock)
                 ++ ")\\}(\\((.*)\\)|$)$" 
blocks = mkRegex regex 

matchBlock = matchRegex blocks

buildReplace (t,n,i) = [(concat ["[",t," ",show i,"]"], link2),
                        (concat ["[",t," ",n,"]"], link2)]
  where link2 = "[" ++ t ++ " " ++ show i ++ "]"
                ++ "(#" ++ t ++ "-" ++ show i++")"

formatTheorem s = replaceMany replaceTable (formatBlocks s)
  where replaceTable = nub $ concatMap buildReplace (blocksAssoc s)

replaceMany [] s = s
replaceMany ((x,y):xs) s = replaceMany xs (replace x y s)

-- Format a block
formatBlocks xs = unlines $ fst $ formatBlocks' (lines xs) 1
blocksAssoc xs = snd $ formatBlocks' (lines xs) 1

formatBlocks' :: [String]->Int->([String],[(String,String,Int)])
formatBlocks' [] _ = ([],[])
formatBlocks' (x:xs) n= ([result] ++ results, assoc++assocs)
  where (result,inc,assoc) = formatBlock x n
        (results, assocs)  = formatBlocks' xs (n+inc)

formatBlock :: String->Int->(String,Int,[(String,String,Int)])
formatBlock x n
 | result    = ("######"++ name ++typeDes,inc, [(bType,name,n),(bType,show n,n)])
 | otherwise = (x,0,[])
 where  result = isJust $ matchBlock x
        [bType,_,name] = fromJust $ matchBlock x
        name' = if null name then "" else "\"" ++  name ++ "\""
        index = if bType `elem` otherBlock then "" else show n
        inc = if bType `elem` otherBlock then 0 else 1
        typeDes = " {type="++ bType ++" index="++ index ++" name=" ++ name' ++ "}"

compute x = (writeDoc $ bottomUp latex $ bottomUp theoremize $ readDoc x) ++ "\n"

latex :: Block -> Block
latex (RawBlock (Format "latex") s) = RawBlock (Format "html") ("<span class=\"math\">" ++s ++ "</span>")
latex x = x

theoremize :: [Block] -> [Block]
theoremize xs = t xs
  where t (x:y:xs)
         | isTheorem x = makeTheorem x y ++ (t xs)
         | otherwise   = x:(t (y:xs))
        t x = x
        
makeTheorem (Header _ (_,_,parm) _) (CodeBlock o xs) = [rawStart,rawHead] ++ content ++ [rawEnd]
  where t = fromJust $ lookup "type" parm
        name = fromJust $ lookup "name" parm
        index = fromJust $ lookup "index" parm
        sectionhead = concat ["<section class=\"theorem-environment ",
                    t,
                    "\" id=\"",
                    t,
                    "-",
                    index,
                    "\">"]
        inittext = "<span class=\"theorem-header\">" ++ typetext ++ indextext ++ nametext ++ "</span>"
        typetext = "<span class=\"type\">" ++ t ++ "</span>" 
        indextext = if null index 
                     then "" 
                     else "<span class=\"index\">" ++ index ++ "</span>"
        nametext = if null name 
                     then "" 
                     else "<span class=\"name\">" ++ (stripParagraph $ mathdocInline name) ++ "</span>"
        end = "</section>"
        rawEnd = RawBlock (Format "html") end
        rawStart = RawBlock (Format "html") sectionhead
        rawHead = RawBlock (Format "html") inittext
        content = (getDoc . readDoc) xs
makeTheorem x y = [x,y]

-- strip <p> and </p> of the beginning to the end of the html.
stripParagraph html = if take 3 html == "<p>" 
                        then take (length html - 8) (drop 3 html)
                        else html

getDoc (Pandoc _ xs) = xs

isTheorem :: Block -> Bool
isTheorem (Header 6 (_, [], param) _) =
    if isJust t
      then (fromJust t) `elem` (incrementBlock ++ otherBlock)
      else False
  where t = lookup "type" param
isTheorem x = False


-- copied missingh's replace implementation, because missingh runs into dependency hell...
-- remove this and uncomment import Data.String.Utils if you can solve this problem somehow
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = intercalate new . split old $ l

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = spanList (not . isPrefixOf delim) str
        in 
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [[]]
                                        else split delim 
                                                 (drop (length delim) x)