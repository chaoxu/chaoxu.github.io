--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import           Data.Monoid (mappend, Monoid (..))
import           Hakyll
import           Hakyll.Core.Compiler
import           Hakyll.Web.Html
import           System.FilePath.Posix
import           Text.Pandoc
import           MathDoc
import           Control.Monad
import           Control.Applicative        ((<$>), Alternative (..), (<$>))
import           Data.Maybe
import           Data.Monoid
--------------------------------------------------------------------------------
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
    match "*.md" $ do
        route $ setExtension "html"
        compile $ mathCompiler
            >>= loadAndApplyTemplate "templates/default.html" postCtx
        --    >>= relativizeUrls
    -- posts
    match "posts/*.md" $ do
        route $ setExtension "html"
        compile $ mathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
        --    >>= relativizeUrls
    -- drafts
    match "drafts/*.md" $ do
        route $ setExtension "html"
        compile $ mathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
        --    >>= relativizeUrls
    -- raw posts
    {-match "posts/*" $ version "raw" $ do
        route   idRoute
        compile copyFileCompiler
    -}
    -- raw files
    match "files/**" $ version "raw" $ do
        route   idRoute
        compile copyFileCompiler
    {-
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList recentFirst) `mappend`
                    constField "title" "Archives"              `mappend`
                    titleField    "htmltitle"                  `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
    -}
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

    -- Index
    match "blog.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ ->
                                postList recentFirst

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
            --    >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

mathDoc :: Item String -> Compiler (Item String)
mathDoc = return . fmap mathdoc

mathCompiler = getResourceBody >>= mathDoc
--mathCompiler = getResourceString >>= mathDoc

idPages = ["favicon.ico",
           "googled46bf4e1cd540289.html",
           "CNAME",
           "index.html"]

htmlTitleField :: Context String
htmlTitleField = Context $ \k _ i -> 
    if (k /= "htmltitle")
    then do empty
    else do value <- getMetadataField (itemIdentifier i) "title"
            return $ StringField (if isNothing value then "" else fromJust value)
                                                                    
betterTitleField :: Context String
betterTitleField = Context $ \k _ i -> 
    if (k /= "title")
    then do empty
    else do value <- getMetadataField (itemIdentifier i) "title"
            return $ StringField (mathdocInline $ if isNothing value then "" else fromJust value)

sourceField key = field key $
    fmap (maybe empty (sourceUrl . toUrl)) . getRoute . itemIdentifier

sourceUrl xs = (take (length xs - 4) xs) ++ "md"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "The Art Gallery Guardian"
  , feedDescription = "Mostly notes on algorithms"
  , feedAuthorName = "Chao Xu"
  , feedAuthorEmail = "mgcclx@gmail.com"
  , feedRoot = "https://chaoxuprime.com/blog"}

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    sourceField "source"  `mappend`
    htmlTitleField        `mappend`
    dateField "date" "%F" `mappend`
    bodyField     "body"  `mappend`
    betterTitleField      `mappend`
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