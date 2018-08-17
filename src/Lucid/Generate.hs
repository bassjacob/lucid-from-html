{-# LANGUAGE PackageImports #-}

-- | A module for conversion from HTML to Lucid Haskell code.
--

module Lucid.Generate (
    module Lucid.Generate
)   where

import "base" Data.List (stripPrefix, intercalate)
import "base" Data.Char (isSpace, showLitChar)

import "tagsoup" Text.HTML.TagSoup
import "tagsoup" Text.HTML.TagSoup.Tree

import Lucid.Sanitize (sanitize, lowerize)
import Lucid.Combinators

data CombinatorType = ParentCombinator
                    | LeafCombinator
                    | UnknownCombinator
                    deriving (Eq, Show)
                    
combinatorF :: HtmlVariant -> Options -> 
                    Row -> String -> [Attribute String] -> String
combinatorF variant opts row tag attrs = 
  case combinatorType variant (lowerize tag) of
    UnknownCombinator ->
      error $ "Line: " ++ show row ++ " tag '"
              ++ tag ++ "' is illegal in "
              ++ show variant
    _ -> tag' ++ attributes' attrs
  where
    tag' = sanitize tag
    attributes' :: Show a => [(String, a)] -> [Char]
    -- hack for <br> that needs attributes in Lucid
    attributes' [] = if sanitize tag `elem` ["br_","hr_"] 
                        then " []"
                        else ""
    attributes' xs =  (" [ " ++) . (++ " ]") . intercalate ", " . fmap displayAttr $ xs
    displayAttr :: Show a => (String, a) -> String
    displayAttr (k, v) = case k `elem` attributes variant of
        True  -> let k' = sanitize k 
                  in case k' of 
                        "autofocus_" -> k'
                        "checked_" -> k'
                        "ngNonBindable_" -> k'
                        _ -> k' ++ " " ++ show v
        False -> case stripPrefix "data-" k of
            Just prefix -> "data_" ++ " "
                        ++ show prefix
                        ++ " " ++ show v
            Nothing | ignore_ opts -> ""
                    | otherwise  -> error $ "Line " ++ show row
                                  ++ ": attribute '"
                                  ++ k ++ "' is illegal in "
                                  ++ show variant

{-
-- | Remove empty text from the HTML.
--
removeEmptyText :: Html -> Html
removeEmptyText (Block b) = Block $ map removeEmptyText $ flip filter b $ \h ->
    case h of Text text -> any (not . isSpace) text
              _         -> True
removeEmptyText (Parent tag attrs inner) | tag `elem` cleanTags =
    Parent tag attrs $ removeEmptyText inner
removeEmptyText x = x
-}

-- | Get the type of a combinator
--
combinatorType :: HtmlVariant -> String -> CombinatorType
combinatorType variant combinator
    | combinator == "docTypeHtml" = ParentCombinator
    | combinator `elem` parents variant = ParentCombinator
    | combinator `elem` leafs variant = LeafCombinator
    | otherwise = UnknownCombinator



-- | Produce the Lucid code from the HTML. The result is a list of lines.
--
fromTagTree :: HtmlVariant    -- ^ Used HTML variant
         -> Options           -- ^ Building options
         -> [TagTree String]  -- ^ HTML tree
         -> [String]          -- ^ Resulting lines of code
fromTagTree variant opts xs =
  concatMap (fromTagTree2 variant opts) (group2 xs)
  where
    group2 (y1:y2:ys) = [y1,y2] : group2 ys
    group2 ys = [ys]


-- | Produce the Lucid code from the HTML. The result is a list of lines.
--
fromTagTree2 :: HtmlVariant    -- ^ Used HTML variant
         -> Options            -- ^ Building options
         -> [TagTree String]   -- ^ HTML tree
         -> [String]           -- ^ Resulting lines of code
fromTagTree2 variant opts [TagLeaf (TagPosition row _),  TagLeaf x] =
  case x of 
    TagText text -> 
      ["\"" ++ escapeString (trim text) ++ "\""]
    TagComment comment ->
      ["toHtmlRaw  \"<!--" ++ escapeString comment ++ "-->\""]
    TagOpen tag attrs ->
      let tag' = lowerize tag
      in
        case combinatorType variant tag' of
          LeafCombinator -> [combinatorF variant opts row tag attrs]
          _  ->
            if tag' == "!doctype"
            then ["doctype_"]
            else
              error $ "Line: " ++ show row ++ " tag '"
                      ++ tag ++ "' is open but nowhere closed "
    _ -> error $ "Line " ++ show row ++ ": " ++ show x ++ " tag is hanging." 
  where
    -- Remove whitespace on both ends of a string
    trim
      | trimText_ opts = reverse . dropWhile isSpace . reverse . dropWhile isSpace
      | otherwise = id

fromTagTree2 variant opts branch@[TagLeaf (TagPosition row _), TagBranch tag attrs inner] =
  let tag' = lowerize tag
  in
    if tag' == "svg"
    then ["toHtmlRaw \"" ++ escapeString (renderTree branch) ++ "\""]
    else
      case combinatorType variant tag' of
          -- Actual parent tags
          ParentCombinator -> 
            let ls = fromTagTree variant opts inner
            in 
              case ls  of
                [] -> [combinator ++ " $ \"\""]
                _  -> (combinator ++ " $ do") :
                      indent ls

          -- Leaf tags
          LeafCombinator -> [combinator]
        -- Unknown tag
          UnknownCombinator -> if ignore_ opts
              then fromTagTree variant opts inner
              else error $ "Line " ++ show row
                           ++ ": tag '" ++ tag ++ "' is illegal in "
                           ++ show variant
  where
    combinator :: String
    combinator = combinatorF variant opts row tag attrs
-- for debug
fromTagTree2 _ _ [TagLeaf (TagPosition _ _)] = []
fromTagTree2 _ _ [] = []
fromTagTree2 _ _ x = error $ "no pattern for: " ++ show x



-- Escape a number of characters
escape :: Char -> ShowS
escape '"'  = showString "\\\""
escape c = showLitChar c

escapeString :: String -> String
escapeString = foldr escape ""


-- | Produce the code needed for initial imports.
--
getImports :: [String]
getImports =
    [ "{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}"
    , ""
    , "import Lucid"
    , "import Lucid.Supplemental"
    ]


-- | Produce the code for IO
--
getIOImports :: [String]
getIOImports = 
    [ "import System.IO (stdout, hSetEncoding, utf8)"
    , "import Data.Text.Lazy.IO as L"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "  hSetEncoding stdout utf8"
    , "  L.hPutStr stdout (renderText template1)"
    , ""
    ]


-- | Convert the HTML to lucid code.
--
lucidFromHtml :: HtmlVariant  -- ^ Variant to use
               -> Options      -- ^ Build options
               -> String       -- ^ Template name
               -> String       -- ^ HTML code
               -> String       -- ^ Resulting code
lucidFromHtml variant opts name =
    unlines . addSignature . fromTagTree variant opts
            . parseTreeOptions parseOptions { optTagPosition = True}
  where
    addSignature body = [ name ++ " :: Html ()"
                        , name ++ " = do"
                        ] ++ indent body


-- | Indent block of code.
--
indent :: [String] -> [String]
indent = map ("    " ++)

-- | The options record passed to 'lucidFromHtml'
--
data Options = Options
             { ignore_     :: Bool -- ^ ignore errors
             , trimText_ :: Bool -- ^ do not trim text
             }
  deriving (Show)

-- Testing
{-

:{
test1 = do
  body <- readFile "tests/issue_11-checked.html"
  let tree = parseTreeOptions parseOptions{ optTagPosition = True } body
  let mainOpts = Options { ignore_ = False, noTrimText_ = True }
  putStrLn $ unlines $ fromTagTree html5S mainOpts tree
:}

-}
