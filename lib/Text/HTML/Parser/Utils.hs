module Text.HTML.Parser.Utils
    ( -- * Conversion
      toToken            -- :: Text -> Token
    , toTokenDefault     -- :: Token -> Text -> Token

      -- * Tag identification
    , isTagOpen          -- :: Token -> Bool
    , isTagClose         -- :: Token -> Bool
    , isTagSelfClose     -- :: Token -> Bool
    , isContentText      -- :: Token -> Bool
    , isContentChar      -- :: Token -> Bool
    , isComment          -- :: Token -> Bool
    , isDoctype          -- :: Token -> Bool
    , isTagOpenName      -- :: Text -> Token -> Bool
    , isTagCloseName     -- :: Text -> Token -> Bool

      -- * Extraction
    , fromContentText    -- :: Token -> Text
    , maybeContentText   -- :: Token -> Maybe Text
    , fromAttrib         -- :: Attr -> Token -> Attr
    , maybeAttrib        -- :: Attr -> Token -> Maybe Attr
    , innerText          -- :: [Token] -> Text
    , toHeadContentText  -- :: [Token] -> Text
    , between            -- :: Token -> Token -> [Token] -> [Token]
    , dropHeader         -- :: [Attr] -> [Token] -> [Token]
    , allContentText     -- :: [Token] -> [Text]

      -- * Utility
    , sections           -- :: (a -> Bool) -> [a] -> [[a]]
    , section            -- :: (a -> Bool) -> [a] -> [a]
    , partitions         -- :: (a -> Bool) -> [a] -> [[a]]

      -- * Combinators
    , (~==)              -- :: Token -> Token -> Bool
    , (~/=)              -- :: Token -> Token -> Bool
    ) where

import qualified Data.Attoparsec.Text as A
import qualified Data.List.NonEmpty   as NE
import qualified Data.Text            as T

import Data.Either (fromRight)
import Data.List (groupBy, tails)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Text.HTML.Parser (
  Attr (Attr),
  Token (Comment, ContentChar, ContentText, Doctype, TagClose, TagOpen, TagSelfClose),
  token,
 )


-- | Like 'toTokenDefault', but with a supplied default value.
toToken :: Text -> Token
toToken = toTokenDefault (Doctype "Could not parse string into token.")

-- | Convert 'Text' to 'Token', with a default in case of a parse failure.
toTokenDefault :: Token -> Text -> Token
toTokenDefault d = fromRight d . A.parseOnly token

-- | This function takes a list, and returns all suffixes whose first item
-- matches the predicate.
sections :: (a -> Bool) -> [a] -> [[a]]
sections p = filter (p . head) . init . tails

-- | Like 'sections', but return the head element.  Returns an empty list if no
-- head element is present.
section :: (a -> Bool) -> [a] -> [a]
section f = \case
  [] -> []
  xs -> maybe [] NE.head (NE.nonEmpty (sections f xs))

-- | This function is similar to 'sections', but splits the list so no element
-- appears in any two partitions.
partitions :: (a -> Bool) -> [a] -> [[a]]
partitions p = groupBy (const notp) . dropWhile notp
 where notp = not . p

-- | Get the first 'ContentText' element from a list of 'Token's.  If no tag
-- could be found, return an empty string.
toHeadContentText :: [Token] -> Text
toHeadContentText = maybe "" NE.head . NE.nonEmpty . allContentText

-- | Get all 'Token's between @start@ and @end@.
between :: Token -> Token -> [Token] -> [Token]
between start end = takeWhile (~/= end  )
                  . drop 1                 -- drop the tag
                  . dropWhile (~/= start)

-- | Drop an HTML header (i.e. the header tags and everything in between), as
-- well as everything before it, from a list of 'Token's.
dropHeader :: [Attr] -> [Token] -> [Token]
dropHeader attr = drop 1                   -- drop </header>
                . dropWhile (~/= TagClose "header"     )
                . dropWhile (~/= TagOpen  "header" attr)

-- | Get all 'ContentText' entries from a list of 'Token's and extract their
-- content.
allContentText :: [Token] -> [Text]
allContentText = mapMaybe maybeContentText

-- | Test if a 'Token' is a 'TagOpen'.
isTagOpen :: Token -> Bool
isTagOpen = \case
  TagOpen{} -> True
  _         -> False

-- | Test if a 'Token' is a 'TagClose'.
isTagClose :: Token -> Bool
isTagClose = \case
  TagClose{} -> True
  _          -> False

-- | Test if a 'Token' is a 'ContentText'.
isContentText :: Token -> Bool
isContentText = \case
  ContentText{} -> True
  _             -> False

-- | Extract the string from within 'ContentText', otherwise return 'Nothing'.
maybeContentText :: Token -> Maybe Text
maybeContentText = \case
  ContentText t -> Just t
  _             -> Nothing

-- | Extract the string from within 'ContentText', crashes if not a
-- 'ContentText'.
fromContentText :: Token -> Text
fromContentText = \case
  ContentText t -> t
  t             -> error $ "(" ++ show t ++ ") is not a ContentText"

-- | Extract all text content from a list of Tokens (similar to Verbatim found
-- in HaXml).
innerText :: [Token] -> Text
innerText = mconcat . mapMaybe maybeContentText

-- | Test if a 'Token' is a 'TagSelfClose'.
isTagSelfClose :: Token -> Bool
isTagSelfClose = \case
  TagSelfClose{} -> True
  _              -> False

-- | Test if a 'Token' is a 'ContentChar'.
isContentChar :: Token -> Bool
isContentChar = \case
  ContentChar{} -> True
  _             -> False

-- | Test if a 'Token' is a 'Comment'.
isComment :: Token -> Bool
isComment = \case
  Comment{} -> True
  _         -> False

-- | Test if a 'Token' is a 'Doctype'.
isDoctype :: Token -> Bool
isDoctype = \case
  Doctype{} -> True
  _         -> False

-- | Returns True if the 'Token' is 'TagOpen' and matches the given name.
isTagOpenName :: Text -> Token -> Bool
isTagOpenName name (TagOpen n _) = n == name
isTagOpenName _    _             = False

-- | Returns True if the 'Token' is 'TagClose' and matches the given name.
isTagCloseName :: Text -> Token -> Bool
isTagCloseName name (TagClose n) = n == name
isTagCloseName _    _            = False

-- | Extract an attribute, crashes if not a 'TagOpen'.  Returns @Attr \"\" \"\"@
-- if no attribute present.
--
-- Warning: does not distinguish between missing attribute and present
-- attribute with values @\"\"@.
--
fromAttrib :: Attr -> Token -> Attr
fromAttrib att tag = fromMaybe (Attr "" "") $ maybeAttrib att tag

-- | Extract an attribute, crashes if not a 'TagOpen'.  Returns
-- @Nothing@ if no attribute present.
maybeAttrib :: Attr -> Token -> Maybe Attr
maybeAttrib att (TagOpen _ atts)
  | att `elem` atts = Just att
  | otherwise       = Nothing
maybeAttrib _ t = error ("(" ++ show t ++ ") is not a TagOpen")

infixl 9 ~==
-- | Performs an inexact match, the first item should be the thing to
-- match.
(~==) :: Token -> Token -> Bool
(~==) = f
 where
  f (ContentText y) (ContentText x) = T.null x             || x == y
  f (TagClose    y) (TagClose    x) = T.null x             || x == y
  f (Comment     x) (Comment     y) = x == mempty          || x == y
  f (TagOpen  y ys) (TagOpen  x xs) = (T.null x || x == y) && all g xs
   where
    g :: Attr -> Bool
    g nv@(Attr name val)
      | T.null name = val  `elem` map (\(Attr o _) -> o) ys
      | T.null val  = name `elem` map (\(Attr _ t) -> t) ys
      | otherwise   = nv   `elem` ys
  f _ _ = False

infixl 9 ~/=
-- | Negation of '(~==)'.
(~/=) :: Token -> Token -> Bool
(~/=) a b = not (a ~== b)
