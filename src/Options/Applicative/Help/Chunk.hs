{-# LANGUAGE FlexibleInstances #-}
module Options.Applicative.Help.Chunk
  ( Chunk, maybeChunk
  , vcatChunks
  , vsepChunks
  , isEmpty
  , paragraph
  , tabulate
  ) where

import Options.Applicative.Help.Pretty
import qualified Prettyprinter.Internal as PP

-- | The free monoid on a semigroup @a@.
type Chunk a = a

maybeChunk :: a -> (Doc -> a) -> Doc -> a
maybeChunk def f doc = case doc of
  PP.Empty -> def
  _ -> f doc

-- | Concatenate 'Chunk's vertically.
vcatChunks :: [Chunk Doc] -> Chunk Doc
vcatChunks = foldr (.$.) mempty

-- | Concatenate 'Chunk's vertically separated by empty lines.
vsepChunks :: [Chunk Doc] -> Chunk Doc
vsepChunks = foldr (\x y -> x .$. mempty .$. y) mempty

-- | Whether a 'Chunk' is empty.  Note that something like 'pure mempty' is not
-- considered an empty chunk, even though the underlying 'Doc' is empty.
isEmpty :: Chunk Doc -> Bool
isEmpty c = case c of
  PP.Empty -> True
  _ -> False

-- | Convert a paragraph into a 'Chunk'.  The resulting chunk is composed by the
-- words of the original paragraph separated by softlines, so it will be
-- automatically word-wrapped when rendering the underlying document.
--
-- This satisfies:
--
-- > isEmpty . paragraph = null . words
paragraph :: String -> Chunk Doc
paragraph = foldr ((</>) . pretty) mempty
          . words

-- | Display pairs of strings in a table.
tabulate :: Int -> [(Doc, Doc)] -> Chunk Doc
tabulate _ [] = mempty
tabulate size table = vcat
  [ indent 2 (fillBreak size key <+> value :: Doc)
  | (key, value) <- table ]
