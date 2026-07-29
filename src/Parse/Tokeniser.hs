{-|
Module      : Parse.Tokeniser
Description : Lexing and tokenization of raw string input.
Author      : Michał Kośka

Breaks down raw user input into distinct strings (tokens), carefully 
managing different quoting states (single, double) and escape characters.
-}

module Parse.Tokeniser where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Char (isLetter, isNumber)
import Types

-- | Tokenises input while expanding $VAR references (unless enclosed in single quotes).
tokeniseInput :: Map.Map T.Text T.Text -> T.Text -> ([T.Text], TokenState)
tokeniseInput vars = go NormalText [] []
  where
    go :: TokenState -> [Char] -> [T.Text] -> T.Text -> ([T.Text], TokenState)
    go state acc tokens txt = case T.uncons txt of
      Nothing -> 
        let finalTokens = if null acc && null tokens
                            then []
                            else reverse (T.pack (reverse acc) : tokens)
        in (finalTokens, state)

      Just (ch, rest) -> case state of
        _ | ch == '$' && state /= SingleQuoteText ->
            case T.uncons rest of
              Just ('{', afterBrace) ->
                let (varName, afterClose) = T.break (== '}') afterBrace
                in case T.uncons afterClose of
                  Just ('}', remainder) ->
                    let val = Map.findWithDefault "" varName vars
                    in go state acc tokens (val <> remainder)
                  _ -> processChar ch rest

              Just (firstChar, _) | isLetter firstChar || firstChar == '_' ->
                let (varName, remainder) = T.span (\c -> isLetter c || isNumber c || c == '_') rest
                    val = Map.findWithDefault "" varName vars
                in go state acc tokens (val <> remainder)

              -- Zwykły znak $
              _ -> processChar ch rest

        _ -> processChar ch rest

      where
        processChar ch rest = case state of
          NormalText -> case ch of
            ' '  -> 
              let newTokens = if null acc then tokens else T.pack (reverse acc) : tokens
              in go NormalText [] newTokens rest
            '\'' -> go SingleQuoteText acc tokens rest
            '"'  -> go DoubleQuotedText acc tokens rest
            '\\' -> go BackslashText acc tokens rest
            _    -> go NormalText (ch : acc) tokens rest

          SingleQuoteText -> case ch of
            '\'' -> go NormalText acc tokens rest
            _    -> go SingleQuoteText (ch : acc) tokens rest

          DoubleQuotedText -> case ch of
            '"'  -> go NormalText acc tokens rest
            '\\' -> go BackslashQuotedText acc tokens rest
            _    -> go DoubleQuotedText (ch : acc) tokens rest

          BackslashText -> 
            go NormalText (ch : acc) tokens rest

          BackslashQuotedText -> 
            if ch `elem` ['$', '"', '\\']
              then go DoubleQuotedText (ch : acc) tokens rest
              else go DoubleQuotedText (ch : '\\' : acc) tokens rest