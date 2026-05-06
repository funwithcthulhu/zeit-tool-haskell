{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Text.German (
  normalizeUmlauts,
  stemGerman,
  tokenizeGerman,
) where

import Data.Char (isLetter, toLower)
import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T

vowels :: [Char]
vowels = "aeiouyäöü"

validSEnding :: [Char]
validSEnding = "bdfghklmnrt"

validStEnding :: [Char]
validStEnding = "bdfghklmnt"

isVowel :: Char -> Bool
isVowel = (`elem` vowels)

replaceUY :: String -> String
replaceUY word = reverse (snd (foldl' step (0, []) word))
 where
  chars = word
  step (idx, acc) ch =
    let prevVowel = idx > 0 && isVowel (chars !! (idx - 1))
        nextVowel = idx < length chars - 1 && isVowel (chars !! (idx + 1))
        nextChar =
          if (ch == 'u' || ch == 'y') && prevVowel && nextVowel
            then toUpperAscii ch
            else ch
     in (idx + 1, nextChar : acc)

  toUpperAscii 'u' = 'U'
  toUpperAscii 'y' = 'Y'
  toUpperAscii other = other

getRegions :: String -> (Int, Int)
getRegions word = (max 3 r1, r2')
 where
  r1 = regionStart 1 word
  r2' = regionStart (max 4 r1) word

  regionStart startIndex chars = go startIndex
   where
    go idx
      | idx >= length chars = length chars
      | not (isVowel (chars !! idx)) && isVowel (chars !! (idx - 1)) = idx + 1
      | otherwise = go (idx + 1)

step1 :: String -> Int -> String
step1 word r1 =
  case firstMatching ["ern", "em", "er", "en", "es", "e"] of
    Just suffix ->
      let trimmed = dropSuffix suffix word
       in if suffix `elem` ["en", "es", "e"] && "niss" `suffixOf` trimmed
            then init trimmed
            else trimmed
    Nothing ->
      if "s" `suffixOf` word && length word - 1 >= r1 && validS (charBeforeSuffix word 1)
        then init word
        else word
 where
  validS (Just ch) = ch `elem` validSEnding
  validS Nothing = False

  firstMatching = findSuffix word r1

step2 :: String -> Int -> String
step2 word r1 =
  case findSuffix word r1 ["est", "en", "er"] of
    Just suffix -> dropSuffix suffix word
    Nothing ->
      if "st" `suffixOf` word
        && length word - 2 >= r1
        && length word >= 6
        && maybe False (`elem` validStEnding) (charBeforeSuffix word 2)
        then dropSuffix "st" word
        else word

step3 :: String -> Int -> Int -> String
step3 word _r1 r2 = go
 where
  go
    | endsInRegion "end" = dropSuffix "end" word
    | endsInRegion "ung" = dropSuffix "ung" word
    | endsInRegion "isch" = dropSuffix "isch" word
    | endsInRegion "ik" = dropSuffix "ik" word
    | endsInRegion "ig" && charBeforeSuffix word 2 /= Just 'e' = dropSuffix "ig" word
    | endsInRegion "lich" = dropSuffix "lich" word
    | endsInRegion "heit" = dropSuffix "heit" word
    | endsInRegion "keit" = dropSuffix "keit" word
    | otherwise = word

  endsInRegion suffix = suffix `suffixOf` word && length word - length suffix >= r2

findSuffix :: String -> Int -> [String] -> Maybe String
findSuffix word region = go
 where
  go [] = Nothing
  go (suffix : rest)
    | suffix `suffixOf` word && length word - length suffix >= region = Just suffix
    | otherwise = go rest

suffixOf :: String -> String -> Bool
suffixOf suffix word = suffix == reverse (take (length suffix) (reverse word))

dropSuffix :: String -> String -> String
dropSuffix suffix word = take (length word - length suffix) word

charBeforeSuffix :: String -> Int -> Maybe Char
charBeforeSuffix word suffixLength
  | length word <= suffixLength = Nothing
  | otherwise = Just (word !! (length word - suffixLength - 1))

normalizeUmlauts :: Text -> Text
normalizeUmlauts =
  T.concatMap replace
 where
  replace 'ä' = "a"
  replace 'ö' = "o"
  replace 'ü' = "u"
  replace 'ß' = "ss"
  replace other = T.singleton other

stemGerman :: Text -> Text
stemGerman raw
  | T.null trimmed = ""
  | T.length trimmed < 2 = normalizeUmlauts trimmed
  | otherwise =
      normalizeUmlauts
        . T.toLower
        . T.pack
        . step3Word
        . step2Word
        . step1Word
        . replaceUY
        . T.unpack
        $ prepped
 where
  trimmed = T.toLower (T.strip raw)
  prepped = T.replace "ß" "ss" trimmed
  (r1, r2) = getRegions (replaceUY (T.unpack prepped))
  step1Word = (`step1` r1)
  step2Word = (`step2` r1)
  step3Word = (\word -> step3 word r1 r2)

tokenizeGerman :: Text -> [Text]
tokenizeGerman input = reverse (finalize current acc)
 where
  (current, acc) = T.foldl' step ("", []) input

  step (token, tokens) ch
    | isGermanLetter ch = (T.snoc token (toLower ch), tokens)
    | otherwise = ("", finalize token tokens)

  finalize token tokens
    | T.length token >= 2 = token : tokens
    | otherwise = tokens

  isGermanLetter ch =
    ch `elem` ("äöüßÄÖÜ" :: String) || isLetter ch
