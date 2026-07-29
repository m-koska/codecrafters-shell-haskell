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
tokeniseInput vars = go NormalText False [] []
  where
    go :: TokenState -> Bool -> [Char] -> [T.Text] -> T.Text -> ([T.Text], TokenState)
    go state inToken acc tokens txt = case T.uncons txt of
      Nothing -> 
        let finalTokens = if inToken
                            then reverse (T.pack (reverse acc) : tokens)
                            else reverse tokens
        in (finalTokens, state)

      Just (ch, rest) -> case state of
        _ | ch == '$' && state /= SingleQuoteText ->
            case T.uncons rest of
              Just ('{', afterBrace) ->
                let (varName, afterClose) = T.break (== '}') afterBrace
                in case T.uncons afterClose of
                  Just ('}', remainder) -> do
                    let val = Map.lookup varName vars
                    case val of
                      Just x -> go state inToken acc tokens (x <> remainder)
                      _      -> go state inToken acc tokens remainder
                    
                  _ -> processChar ch rest

              Just (firstChar, _) | isLetter firstChar || firstChar == '_' -> do
                let (varName, remainder) = T.span (\c -> isLetter c || isNumber c || c == '_') rest
                let val = Map.lookup varName vars
                case val of 
                  Just x -> go state inToken acc tokens (x <> remainder)
                  _      -> go state inToken acc tokens remainder

              -- Zwykły znak $
              _ -> processChar ch rest

        _ -> processChar ch rest

      where
        processChar ch rest = case state of
          NormalText -> case ch of
            ' '  -> 
              let newTokens = if inToken then T.pack (reverse acc) : tokens else tokens
              in go NormalText False [] newTokens rest
            '\'' -> go SingleQuoteText True acc tokens rest
            '"'  -> go DoubleQuotedText True acc tokens rest
            '\\' -> go BackslashText True acc tokens rest
            _    -> go NormalText True (ch : acc) tokens rest

          SingleQuoteText -> case ch of
            '\'' -> go NormalText True acc tokens rest
            _    -> go SingleQuoteText True (ch : acc) tokens rest

          DoubleQuotedText -> case ch of
            '"'  -> go NormalText True acc tokens rest
            '\\' -> go BackslashQuotedText True acc tokens rest
            _    -> go DoubleQuotedText True (ch : acc) tokens rest

          BackslashText -> 
            go NormalText True (ch : acc) tokens rest

          BackslashQuotedText -> 
            if ch `elem` ['$', '"', '\\']
              then go DoubleQuotedText True (ch : acc) tokens rest
              else go DoubleQuotedText True (ch : '\\' : acc) tokens rest