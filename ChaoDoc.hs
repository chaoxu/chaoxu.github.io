module ChaoDoc ( chaoDocRead, chaoDocWrite, theoremFilter, chaoDocInline) where
import Text.Pandoc
import Text.Regex (mkRegex, matchRegex)
import Data.Maybe
import Text.Pandoc.Writers.HTML
import Data.Text (Text, unpack, pack)
import Data.Set (insert)
import System.Environment (getArgs)
import Text.CSL.Pandoc
import System.IO.Unsafe
import Data.Array((!))
import Data.Bits((.|.))
import Data.Either
import Text.Pandoc.Builder
import Text.Pandoc.Walk (walk,walkM,query)
import Text.Pandoc.Definition
import Data.List
import qualified Data.Map as M
import Control.Monad.State

setMeta key val (Pandoc (Meta ms) bs) = Pandoc (Meta $ M.insert key val ms) bs

-- On mac, please do `export LANG=C` before using this thing
chaoDocRead = def{readerExtensions = enableExtension Ext_tex_math_double_backslash $
                                     enableExtension Ext_tex_math_single_backslash $ 
                                     enableExtension Ext_raw_tex $ pandocExtensions}
chaoDocWrite = def{writerHTMLMathMethod = KaTeX "",
                   -- writerHtml5          = True,
                   --writerHighlightStyle = Just syntaxHighlightingStyle,
                   writerNumberSections = True}

chaoDocToPandoc :: String -> Pandoc
chaoDocToPandoc x = fromRight (Pandoc nullMeta []) $ runPure (readMarkdown chaoDocRead (pack x))

getInline :: Inline -> [Inline]
getInline x = [x]

pandocToInline :: Pandoc -> [Inline]
pandocToInline x = query getInline x

writeDocT :: Pandoc -> String
writeDocT x = unpack $ fromRight (pack "") $ runPure $ writeHtml5String chaoDocWrite x

chaoDocInline :: String->String
chaoDocInline x = removeP $ writeDocT $ chaoDocToPandoc x
  where removeP x = drop 3 (take (length x - 4) x) 

incrementalBlock = ["Theorem",
                  "Conjecture",
                  "Definition",
                  "Example",
                  "Lemma",
                  "Problem",
                  "Proposition",
                  "Corollary"]
otherBlock = ["Proof","Remark"]             
theoremClasses = incrementalBlock ++ otherBlock

-- create a filter for theorems
getClass :: Attr -> [String]
getClass (_,c,_) = c

addClass :: Attr -> String -> Attr
addClass (a,b,c) d = (a,d:b,c)

addAttr :: Attr -> String -> String -> Attr
addAttr (a,b,c) x y = (a,b,(x,y):c)

-- For each theorem, add a number, and also add add class theorem
preprocessTheorems :: Block -> State Int Block
preprocessTheorems (Div attr xs) 
 | isIncremental = do
          curId <- get
          put (curId + 1)
          return $ Div (addAttr attr' "index" (show curId)) xs
 | isOtherBlock = return $ Div attr' xs
 | otherwise = return (Div attr xs)
 where isIncremental= getClass attr `intersect` incrementalBlock /= []
       isOtherBlock= getClass attr `intersect` otherBlock /= []
       theoremType = head (getClass attr `intersect` theoremClasses)
       attr' = addAttr attr "type" theoremType
preprocessTheorems x = return x

theoremFilter :: Pandoc -> Pandoc
theoremFilter doc = walk makeTheorem $ autorefFilter $ evalState (walkM preprocessTheorems doc) 1

--    [index, type, id]
theoremIndex :: Block -> [(String, (String, String))]
theoremIndex (Div attr xs) 
 | isNothing t = []
 | isIncremental = [(id,(fromJust t,fromJust index))]
 | otherwise = []
 where (id,_,parm) = attr
       t = lookup "type" parm
       index = lookup "index" parm
       isIncremental = fromJust t `elem` incrementalBlock
theoremIndex _ = []

autoref :: [(String, (String, String))] -> Inline -> Inline
autoref x (Cite citations inlines)
 | valid     = Link nullAttr [Str linkTitle] ("#"++citeid, linkTitle)
 | otherwise = Cite citations inlines
 where citeid = citationId $ head citations 
       valid = citeid `elem` map fst x
       (theoremType,num) = fromJust $ lookup citeid x
       linkTitle = theoremType ++" "++ num
autoref _ y = y

autorefFilter :: Pandoc -> Pandoc
autorefFilter x = walk (autoref links) x
 where links = query theoremIndex x


makeTheorem :: Block -> Block
makeTheorem (Div attr xs)
 | isNothing t = Div attr xs
 | otherwise = Div (addClass attr "theorem-environment") (Plain [header]:xs)
 where (_,_,parm) = attr 
       t = lookup "type" parm
       name = lookup "title" parm
       index = lookup "index" parm
       header = Span (addClass nullAttr "theorem-header") [typetext,indextext,nametext]
       typetext = Span (addClass nullAttr "type") [Str $ fromJust t]
       indextext = if isNothing index 
                      then Str ""
                      else Span (addClass nullAttr "index") [Str $ fromJust index]
       nametext = if isNothing name
                     then Str ""
                     else Span (addClass nullAttr "name") (pandocToInline $ chaoDocToPandoc $ fromJust name) -- titles should be rich
makeTheorem x = x