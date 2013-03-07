{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | Simple pagination support for blaze.

module Text.Blaze.Pagination where

import           Data.Foldable
import           Control.Monad hiding (forM_)
import           Data.Monoid.Operator
import           Data.Pagination
import           Network.URI
import qualified Prelude                     as P
import           Prelude                     hiding ((++),div,span)
import           Text.Blaze.Extra
import           Text.Blaze.Html5            as H hiding (map)

data PN = PN { pnURI :: URI
             , pnPn :: Pagination
             , pnResultsPerPage :: Maybe [Integer]
             }

-- | Render pagination as html.
pagination :: PN -> Html
pagination PN{pnURI=uri, pnPn=pn@Pagination{..}, pnResultsPerPage=perPage} =
  div !. "pagination" $ do
    when pnShowDesc description
    forM_ perPage $ resultsPerPage
    chooser

  where description = do
         span !. "description" $ do
           "Showing "
           toHtml ((pnCurrentPage-1)*pnPerPage + 1)
           "–"
           toHtml (min pnTotal (pnCurrentPage * pnPerPage))
           " of "
           toHtml (pnTotal)
           " results"

        resultsPerPage perPage = do
          div !. "results-per-page" $ do
            "Page size: "
            forM_ perPage $ \count ->
              span !. "per-page-choice" $ do
                let theclass = if count == pnPerPage then "current" else ""
                a !. theclass ! hrefSet uri (param "per_page") (show count) $
                  toHtml (show count)

        chooser = do
          div !. "pages" $ do
            ul !. "pages-list" $ do
              when (pnCurrentPage > 1) $ do
                li !. "page" $ a ! hrefSet uri paramName (show 1) $
                  "First"
                li !. "page" $ a ! hrefSet uri paramName (show (pnCurrentPage-1)) $
                  "Previous"
              let w = 10 :: Integer
                  start = max 1 (pnCurrentPage - (w // 2))
                  end = min (pageCount) (start + w)
              forM_ [start..end] $ \i ->
                li !. "page" $ do
                  let theclass = if i == pnCurrentPage then "current" else ""
                  a ! hrefSet uri paramName (show i) !. theclass $
                    toHtml (show i)
              when (end < pageCount) $
                li !. "page" $ "…"
              when (pnCurrentPage < pageCount) $ do
                li !. "page" $ a ! hrefSet uri paramName (show (pnCurrentPage+1)) $
                  "Next"
                li !. "page" $ a ! hrefSet uri paramName (show pageCount) $
                  "Last"

        paramName = param "page"
        param p = pnName ++ "_" ++ p

        (//) = P.div
        pageCount = pnPageCount pn
