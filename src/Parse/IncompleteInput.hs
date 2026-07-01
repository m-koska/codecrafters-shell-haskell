module Parse.IncompleteInput where

import Types

-- a simple longest commpn prefix algorithm, for tab completions  
commonPrefix :: String -> String -> String
commonPrefix (x:xs) (y:ys) 
  | x == y    = x : commonPrefix xs ys
commonPrefix _ _ = ""

longestCommonPrefix :: [String] -> String
longestCommonPrefix = foldl1 commonPrefix

splitFilePath :: String -> (String, String)
splitFilePath token =
  let rev_text     = reverse token
      (name, path) = break (== '/') rev_text
  in (reverse path, reverse name)

getLastWordContext :: String -> (String, TokenState)
getLastWordContext input = 
  let (reverse_words, state) = go NormalText "" input
  in (reverse reverse_words, state)

  where
    go :: TokenState -> String -> String -> (String, TokenState)
    
    -- 1. end of input
    go state acc [] = (acc, state)
    -- 1.5 space outside quoting -> read the next token
    go NormalText acc (' ':xs) = go NormalText "" xs

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