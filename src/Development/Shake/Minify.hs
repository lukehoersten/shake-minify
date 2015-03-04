-- | Minify JS and CSS files using no external @$PATH@ dependencies.
--
-- @
-- main :: IO ()
-- main = shakeArgs shakeOptions $ do
--     want ["\/\/*.min.js", "\/\/*.min.css"]
--     "//*.min.js"   *> minifyJs
--     "//*.min.css"  *> minifyCss
-- @

module Development.Shake.Minify where

import qualified Data.ByteString.Lazy       as BS
import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as LTB
import           Development.Shake          (Action, liftIO, need)
import           Development.Shake.FilePath (dropExtension, (-<.>))
import qualified Text.CSS.Parse             as CSS
import qualified Text.CSS.Render            as CSS
import qualified Text.Jasmine               as JS


-- | Given a @.min.js@ path, find the @.js@ file and minify it into the specified file name.
minifyJs :: FilePath -- ^ Desired minified JS files (ex: @"//*.min.js"@)
         -> Action ()
minifyJs = minifyCss' ((-<.> "js") . dropExtension)


-- | Same as `minifyJs` except take a function for custom file path mapping.
minifyJs' :: (FilePath -> FilePath) -- ^ Given a target minified JS file path, return the source JS file path.
          -> FilePath               -- ^ Desired minified JS file (ex: @"//*.min.js"@)
          -> Action ()
minifyJs' fromMin minJs = do
    let js = fromMin minJs
    need [js]
    liftIO $ BS.writeFile minJs =<< JS.minifyFile js


-- | Given a @.min.css@ path, find the @.css@ file and minify it into the specified file name.
minifyCss :: FilePath -- ^ Desired minified CSS file (ex: @"//*.min.css"@)
          -> Action ()
minifyCss = minifyCss' ((-<.> "css") . dropExtension)


-- | Same as `minifyCss` except take a function for custom file path mapping.
minifyCss' :: (FilePath -> FilePath) -- ^ Given a target minified CSS file path, return the source CSS file path.
           -> FilePath               -- ^ Target minified CSS file (ex: @"//*.min.css"@)
           -> Action ()
minifyCss' fromMin minCss = do
    let css = fromMin minCss
    need [css]
    liftIO $ TIO.writeFile minCss . LT.toStrict . LTB.toLazyText . minify =<< TIO.readFile css
  where minify = either error CSS.renderNestedBlocks . CSS.parseNestedBlocks
