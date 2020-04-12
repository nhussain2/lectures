module MP.MP3Spec (spec) where

import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Control.Exception
import Data.Char
import Control.Monad.Writer
import MP.MP3 hiding (htmlDoc1, htmlDoc2, htmlDoc3, htmlDoc4, execParser)


spec :: Spec
spec = describe "MP3" $ do
  describe "pHtml" $ do
    it "works on sample HTML documents" $ do
      forM_ (zip htmlDocs htmlDocTrees) $ \(doc,tree) -> do
        execParser pHtml doc `shouldBe` tree
    it "fails on invalid HTML documents" $ do
      pendingWith "flesh out this test!"

  describe "genHtml" $ do
    it "inverses the work of the parser" $ do
      forM_ htmlDocs $ \doc -> do
        let tree = execParser pHtml doc
        tree `shouldBe` execParser pHtml (execWriter $ genHtml tree)
    it "fails on ill-formed HTML doctrees" $ do
      pendingWith "flesh out this test!"


execParser :: Parser a -> String -> a
execParser p s = let Just (_, v) = parse p s in v

htmlDocs = [htmlDoc1, htmlDoc2, htmlDoc3, htmlDoc4]
htmlDocTrees = [htmlDocTree1, htmlDocTree2, htmlDocTree3, htmlDocTree4]

htmlDoc1 = "<!DOCTYPE html><html></html>"
htmlDocTree1 = Element {elemName = "html", elemContent = []}

htmlDoc2 = "<!DOCTYPE html>\
           \<html>\
           \  <head></head>\
           \  <body>\
           \    <div>\
           \    </div>\
           \  </body>\
           \</html>"
htmlDocTree2 = Element {elemName = "html", elemContent = [Element {elemName = "head", elemContent = []},Element {elemName = "body", elemContent = [Element {elemName = "div", elemContent = []}]}]}

htmlDoc3 = "<!DOCTYPE html>\
           \<html lang=\"en\">\
           \  <head>\
           \    <title>CS 340</title>\
           \  </head>\
           \  <body>\
           \    <div id=\"content\">\
           \      <p>Hello world!</p>\
           \    </div>\
           \  </body>\
           \</html>"
htmlDocTree3 = Element {elemName = "html", elemContent = [Attrib {attribName = "lang", attribVal = "en"},Element {elemName = "head", elemContent = [Element {elemName = "title", elemContent = [Text {textContent = "CS 340"}]}]},Element {elemName = "body", elemContent = [Element {elemName = "div", elemContent = [Attrib {attribName = "id", attribVal = "content"},Element {elemName = "p", elemContent = [Text {textContent = "Hello world!"}]}]}]}]}

htmlDoc4 = "<!DOCTYPE html>\
           \<html lang=\"en\">\
           \  <head>\
           \    <title>CS 340</title>\
           \  </head>\
           \  <body>\
           \    <div id=\"content\" class=\"row\">\
           \      <p>This is a <strong>cool</strong> paragraph.</p>\
           \      <p>\
           \        This one is <span style=\"font-family: Helvetica;\">\
           \        not so cool.</span>\
           \      </p>\
           \    </div>\
           \    <div id=\"footer\">\
           \      <p>&copy; 2020</p>\
           \    </div>\
           \  </body>\
           \</html>"
htmlDocTree4 = Element {elemName = "html", elemContent = [Attrib {attribName = "lang", attribVal = "en"},Element {elemName = "head", elemContent = [Element {elemName = "title", elemContent = [Text {textContent = "CS 340"}]}]},Element {elemName = "body", elemContent = [Element {elemName = "div", elemContent = [Attrib {attribName = "id", attribVal = "content"},Attrib {attribName = "class", attribVal = "row"},Element {elemName = "p", elemContent = [Text {textContent = "This is a "},Element {elemName = "strong", elemContent = [Text {textContent = "cool"}]},Text {textContent = "paragraph."}]},Element {elemName = "p", elemContent = [Text {textContent = "This one is "},Element {elemName = "span", elemContent = [Attrib {attribName = "style", attribVal = "font-family: Helvetica;"},Text {textContent = "not so cool."}]}]}]},Element {elemName = "div", elemContent = [Attrib {attribName = "id", attribVal = "footer"},Element {elemName = "p", elemContent = [Text {textContent = "&copy; 2020"}]}]}]}]}
