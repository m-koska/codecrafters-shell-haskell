module Parse.Tokeniser where

import qualified Data.Text as T

import Types

tokeniseInput :: T.Text -> [T.Text]
tokeniseInput input = 
  let 
    input_string = T.unpack input
    tokens_raw = go NormalText "" input_string
    tokens_filtered = filter (not . null) tokens_raw
    tokens_right_order = map reverse tokens_filtered 

  in map T.pack tokens_right_order
  
  where 
    -- TokenState - qouting etc
    -- String     - current accumulated argument
    -- String     - remaining text
    go :: TokenState -> String -> String -> [String]
    -- 1. the current character is a "" (nothing) or a space
    go _ acc [] = [acc]
    -- add finished word to the end of the list
    go NormalText acc (' ':xs) = acc : go NormalText "" xs

    -- 2. special caracters - state change
    go NormalText acc ('\'':xs) = go SingleQuoteText acc xs
    go NormalText acc ('"':xs)  = go DoubleQuotedText acc xs
    go NormalText acc ('\\':xs) = go BackslashText acc xs

      -- 3. SingleQuoteText
    go SingleQuoteText acc ('\'':xs) = go NormalText acc xs
    go SingleQuoteText acc (x:xs)    = go SingleQuoteText (x:acc) xs

    --  4. DoubleQuotedText
    go DoubleQuotedText acc ('"':xs) = go NormalText acc xs
    go DoubleQuotedText acc ('\\':xs) = go BackslashQuotedText acc xs
    go DoubleQuotedText acc (x:xs) = go DoubleQuotedText (x:acc) xs

    -- 5. BackslashText
    go BackslashText acc (x:xs) = go NormalText (x:acc) xs
    --go BackslashQuotedText acc (x:xs) = go DoubleQuotedText (x:acc) xs
    go BackslashQuotedText acc (x:xs)
      | x `elem` ['$', '"', '\\'] = go DoubleQuotedText (x:acc) xs -- chars affected by backslash \
      | otherwise                 = go DoubleQuotedText (x:'\\':acc) xs

    -- default go (take 'x' and glue it in front of 'acc'):
    go NormalText acc (x:xs) = go NormalText (x:acc) xs