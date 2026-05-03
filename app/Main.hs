{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (fromGregorian, getCurrentTime)
import ZeitLingq.App.Model (Model(..), initialModel)
import ZeitLingq.App.Update (Event(..), update)
import ZeitLingq.Core.KnownWords (estimateKnownPct, importKnownWordStems)
import ZeitLingq.Domain.Article (composeCleanText, lessonTitle, wordCount)
import ZeitLingq.Domain.Section (allSections)
import ZeitLingq.Domain.Types

main :: IO ()
main = do
  now <- getCurrentTime
  let demoArticle =
        Article
          { articleId = Just (ArticleId 1)
          , articleUrl = "https://www.zeit.de/wissen/2026-05/beispiel"
          , articleTitle = "Beispielartikel"
          , articleSubtitle = "Ein Haskell-first Umbau"
          , articleAuthor = "Codex"
          , articleDate = Just "2026-05-02"
          , articleSection = "Wissen"
          , articleParagraphs =
              [ "Das ist ein kurzer Beispieltext fur die erste Migrationsstufe."
              , "Wir portieren zuerst das reine Kernverhalten und hangen danach eine GUI an."
              ]
          , articleFetchedAt = Just now
          , articleUploadedLesson = Nothing
          , articleIgnored = False
          , articleAudioUrl = Nothing
          , articleAudioPath = Nothing
          , articleKnownPct = Nothing
          }
      demoDay = fromGregorian 2026 5 2
      knownWords = importKnownWordStems "das\nkurz\ntext\ngui\nhangen"
      knownPct = estimateKnownPct knownWords (composeCleanText demoArticle)
      (nextModel, commands) =
        update
          (SectionCollectionsChanged (Map.fromList [("Wissen", "science-collection")]))
          initialModel

  putStrLn "Zeit LingQ Haskell migration scaffold"
  putStrLn "==================================="
  putStrLn ("Sections ported: " <> show (length allSections))
  putStrLn ("Demo article word count: " <> show (wordCount demoArticle))
  putStrLn ("Known-word estimate: " <> show knownPct)
  putStrLn ("LingQ lesson title: " <> T.unpack (lessonTitle demoDay True (articleTitle demoArticle)))
  putStrLn ("Clean text preview: " <> take 80 (T.unpack (composeCleanText demoArticle)) <> "...")
  putStrLn ("Current view in pure model: " <> show (currentView nextModel))
  putStrLn ("Pending pure commands: " <> show commands)
