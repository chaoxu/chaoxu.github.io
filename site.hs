--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
import           Data.Monoid (mappend, Monoid (..))
import           Hakyll
import           Hakyll.Core.Compiler
import           Hakyll.Web.Html
import           System.FilePath.Posix
import           Text.Pandoc
import           ChaoDoc
import           Control.Monad
import           Control.Applicative        ((<$>), Alternative (..), (<$>))
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text, unpack, pack)
import qualified Data.Map as M
import           Hakyll.Web.Pandoc.Biblio 
-- import qualified Text.CSL as CSL
import           Text.Pandoc.Options
import           Text.Pandoc.Citeproc
import           System.IO.Unsafe
import           Data.Either
--------------------------------------------------------------------------------

cslFile = "bib_style.csl" 
bibFile = "reference.bib" 
-- Must change the main matches

main :: IO ()
main = hakyll $ do
    -- static resources
    match (fromList idPages) $ do
        route   idRoute
        compile copyFileCompiler
    -- css
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    -- pages
    match "pages/*.md" $ do
        route   $ setExtension ".html"
        compile $ do 
            chaoDocCompiler
              >>= loadAndApplyTemplate "templates/default.html" postCtx
              >>= katexFilter
    -- some pages not in pages for some reason (looking at you, README.md, 404.md)
    match "*.md" $ do
        route   $ setExtension ".html"
        compile $ do 
            chaoDocCompiler
              >>= loadAndApplyTemplate "templates/default.html" postCtx
              >>= katexFilter
    -- posts
    match "posts/*.md" $ do
        route   $ setExtension ".html"
        compile $ do
            chaoDocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= katexFilter

    -- cnposts
    match "cnposts/*.md" $ do
        route   $ setExtension ".html"
        compile $ do
            chaoDocCompiler
                >>= loadAndApplyTemplate "templates/cnpost.html" postCtx
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/cndefault.html" postCtx
                >>= katexFilter

    -- drafts
    {-
    match "drafts/*.md" $ do
        route $ setExtension "html"
        compile $ (loadAndApplyTemplate "templates/post.html"    postCtx)
            >>= loadAndApplyTemplate "templates/default.html" postCtx
        --    >>= relativizeUrls
    -}
    -- raw posts
    {-match "posts/*" $ version "raw" $ do
        route   idRoute
        compile copyFileCompiler
    -}
    -- raw files
    match "files/**" $ version "raw" $ do
        route   idRoute
        compile copyFileCompiler
    -- sitemap
    
    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            -- load and sort the posts
            posts <- recentFirst =<< loadAll "posts/*"
                           -- mappend the posts and singlePages together
            let pages = posts 
                           -- create the `pages` field with the postCtx
                           -- and return the `pages` value for it
                sitemapCtx = listField "pages" postCtx (return pages)
            -- make the item and apply our sitemap template
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    -- rss 
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedCtx posts

    -- cnrss 
    create ["cnrss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "cnposts/*" "content"
            renderRss feedConfiguration feedCtx posts

    -- Index
    match "blog.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ ->
                                postList recentFirst

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= katexFilter
            --    >>= relativizeUrls

    -- Index
    match "cnblog.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "cnposts" $ \_ ->
                                cnpostList recentFirst

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/cndefault.html" postCtx
                >>= katexFilter

    -- bib

    match "bib_style.csl" $ compile cslCompiler
    match "reference.bib" $ compile biblioCompiler

    match "templates/*" $ compile templateCompiler

katexFilter = withItemBody (unixFilter "./katex_cli" [])

idPages = ["favicon.ico",
           "googled46bf4e1cd540289.html",
           "CNAME",
           "index.html",
           ".nojekyll"]

htmlTitleField :: Context String 
htmlTitleField = Context $ \k _ i -> 
    if (k /= "htmltitle")
    then do empty
    else do value <- getMetadataField (itemIdentifier i) "title"
            return $ StringField (if isNothing value then "" else fromJust value)
                                                                    
betterTitleField :: Context String
betterTitleField = Context $ \k _ i -> 
    if (k /= "richtitle")
    then do empty
    else do value <- getMetadataField (itemIdentifier i) "title"
            return $ StringField (chaoDocInline $ if isNothing value then "" else (pack $ fromJust value))

sourceField key = field key $
    fmap (maybe empty (sourceUrl . toUrl)) . getRoute . itemIdentifier

sourceUrl xs = (take (length xs - 4) xs) ++ "md"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "The Art Gallery Guardian"
  , feedDescription = "Mostly notes on algorithms"
  , feedAuthorName = "Chao Xu"
  , feedAuthorEmail = "mgcclx@gmail.com"
  , feedRoot = "https://chaoxuprime.com"}

--------------------------------------------------------------------------------
postCtx :: Context String 
postCtx =
    sourceField "source"  `mappend`
    dateField "date" "%F" `mappend`
    bodyField     "body"  `mappend`
    betterTitleField      `mappend`
    htmlTitleField        `mappend`
    defaultContext        `mappend`
    constField "tags"  "" `mappend`
    missingField
--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String 
postList sortFilter = do
    posts   <- sortFilter =<< loadAll ("posts/*" .&&. hasNoVersion)
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

--------------------------------------------------------------------------------
cnpostList :: ([Item String] -> Compiler [Item String]) -> Compiler String 
cnpostList sortFilter = do
    posts   <- sortFilter =<< loadAll ("cnposts/*" .&&. hasNoVersion)
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list


--------------------------------------------------------------------------------
chaoDocCompiler :: Compiler (Item String)
chaoDocCompiler = do
    getResourceBody >>=
        myReadPandocBiblio chaoDocRead (pack cslFile) (pack bibFile) theoremFilter >>=
        return . writePandocWith chaoDocWrite

-- chaoDocCompiler = pandocCompilerWithTransform chaoDocRead chaoDocWrite theoremFilter

addMeta name value (Pandoc meta a) =
  let prevMap = unMeta meta
      newMap = M.insert name value prevMap
      newMeta = Meta newMap
  in  Pandoc newMeta a

myReadPandocBiblio :: ReaderOptions
                   -> Text  -- csl file name
                   -> Text
                   -> (Pandoc -> Pandoc)           -- apply a filter before citeproc
                   -> Item String
                   -> Compiler (Item Pandoc)
myReadPandocBiblio ropt csl biblio filter item  = do
    -- Parse CSL file, if given
    -- style <- unsafeCompiler $ CSL.readCSLFile Nothing . toFilePath . itemIdentifier $ csl

    -- We need to know the citation keys, add then *before* actually parsing the
    -- actual page. If we don't do this, pandoc won't even consider them
    -- citations!
    -- let Biblio refs = itemBody biblio
    pandoc <- itemBody <$> readPandocWith ropt item
    let pandoc' = fromRight pandoc $ unsafePerformIO $ runIO $ processCitations $ addMeta "bibliography" (MetaList [MetaString biblio]) $ 
                  addMeta "csl" (MetaString csl) $ 
                  addMeta "link-citations" (MetaBool True) $ 
                  addMeta "reference-section-title" (MetaInlines [Str "References"]) $
                  filter pandoc -- here's the change
    --let a x = itemSetBody (pandoc' x) 
    return $ fmap (const pandoc') item
