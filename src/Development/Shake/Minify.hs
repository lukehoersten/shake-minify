-- | Minify JS and CSS files similar to the following example:
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
minifyJs minJs = do
    let js = dropExtension minJs -<.> "js"
    need [js]
    liftIO $ BS.writeFile minJs =<< JS.minifyFile js


-- | Given a @.min.css@ path, find the @.css@ file and minify it into the specified file name.
minifyCss :: FilePath -- ^ Desired minified CSS files (ex: @"//*.min.css"@)
          -> Action ()
minifyCss minCss = do
    let css = dropExtension minCss -<.> "css"
    need [css]
    liftIO $ TIO.writeFile minCss . LT.toStrict . LTB.toLazyText . minify =<< TIO.readFile css
  where minify = either error CSS.renderNestedBlocks . CSS.parseNestedBlocks
