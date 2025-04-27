{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
| * 静的ファイルのハンドラをソースコード中に埋め込むための関数群を提供する.
| Devevelopビルド環境では、静的ファイルの内容をリクエストの度に返すような関数を生成する.
| Productioビルド環境では、静的ファイルの内容をHaskellソースに埋め込む関数を生成する.
| Prodctionビルド環境にするには、環境変数RUNTIME_ENV=Prodを設定する
-}
module Jabara.Servant.Embedded (
  genHtmlHandlerImpl,
  genStaticFileHandlerImpl,
  defHtmlHandler,
  defStaticFileHandler,
  HTML (..),
  FileContent,
) where

import Control.Monad.IO.Class
import Data.ByteString.Lazy as Lazy (ByteString, readFile)
import Data.FileEmbed
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import Network.HTTP.Media ((//), (/:))
import Servant
import System.Directory (getDirectoryContents)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import qualified Data.Maybe as Maybe

{- |
| ファイル内容を返すための定義
|
-}
newtype FileContent = FileContent {unRaw :: Lazy.ByteString}

data HTML = HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML FileContent where
  mimeRender _ = unRaw

{- |
| 可読性向上のためのエイリアス.
|
-}
type FunctionName = String

{- | 指定のパスのHTMLファイルの中身を返すHandlerの実装を埋め込む.
|
| 開発ビルド
> htmlHandlerDynamic "static/index.html"
|
| プロダクションビルド
> htmlHandlerEmbedded <htmlファイル内容のLazy.ByteString表現>
-}
genHtmlHandlerImpl :: FilePath -> Q Exp
genHtmlHandlerImpl htmlPath = do
  isProd <- liftIO $ isProductionEnv
  if not isProd
    then [|htmlHandlerDynamic htmlPath|]
    else do
      htmlExp <- embedTextContent htmlPath
      [|htmlHandlerEmbedded $(return htmlExp)|]

{- |
| HTMLファイルのハンドラ関数を作成する.
|
| 開発ビルド
> indexHtmlHandler :: Handler FileContent
> indexHtmlHandler = htmlHandlerDynamic "static/index.html"
|
| プロダクションビルド
> indexHtmlHandler :: Handler FileContent
> indexHtmlHandler = htmlHandlerEmbedded <htmlファイル内容のLazy.ByteString表現>
-}
defHtmlHandler :: FunctionName -> FilePath -> Q [Dec]
defHtmlHandler functionName pathWithSpace = do
  implExp <- genHtmlHandlerImpl $ strip pathWithSpace
  return
    [ htmlHandlerSigniture functionName
    , FunD
        (mkName functionName)
        [ Clause
            []
            (NormalB implExp)
            []
        ]
    ]

{- | 指定のディレクトリ直下のファイルの中身を返すHandlerの実装を埋め込む.
|
| 開発ビルド
> sreveDirectoryWebApp "static"
|
| プロダクションビルド
> serveDirectoryEmbedded [
>     ("app.js", <ファイル内容のLazy.ByteString表現>)
>   , ("app.css", <ファイル内容のLazy.ByteString表現>)
>   ]
-}
genStaticFileHandlerImpl :: FilePath -> Q Exp
genStaticFileHandlerImpl directoryPath = do
  isProd <- liftIO $ isProductionEnv
  if not isProd
    then [|serveDirectoryWebApp directoryPath|]
    else do
      files <- liftIO $ listStaticFileNames directoryPath
      tupleExps <-
        mapM
          ( \fileName -> do
              bsExp <- Data.FileEmbed.embedFile (directoryPath </> fileName)
              return $ TupE [Just $ LitE $ StringL fileName, Just bsExp]
          )
          files
      let embeddedFilesListExp = ListE tupleExps
      [|serveDirectoryEmbedded $(return embeddedFilesListExp)|]

{- |
| HTML以外の静的ファイルを返すハンドラを生成する関数群.
|
| 開発ビルド
> staticFileHandler :: ServerT Raw m
> staticFileHandler = sreveDirectoryWebApp "static"
|
| プロダクションビルド
> staticFileHandler :: ServerT Raw m
> staticFileHandler = serveDirectoryEmbedded [
>     ("app.js", "<ByteString literal>")
>   , ("app.css", "<ByteString literal>")
>   ]
-}
defStaticFileHandler :: FunctionName -> FilePath -> Q [Dec]
defStaticFileHandler functionName directoryPath = do
  let funName = mkName functionName
  implExp <- genStaticFileHandlerImpl directoryPath
  return
    [ SigD
        funName
        (AppT (AppT (ConT ''ServerT) (ConT ''Raw)) (VarT $ mkName "m")) -- ServerT Raw m
    , FunD
        funName
        [ Clause
            []
            (NormalB implExp)
            []
        ]
    ]

{- |
| ヘルパー関数
|
-}
isProductionEnv :: IO Bool
isProductionEnv = lookupEnv "RUNTIME_ENV" >>= return . Maybe.maybe False ((==) "Prod")

-- FilePathの内容のData.ByteString.Lazy.ByteString表現.
-- Lazyである点がData.FileEmbed.embedFileと異なる.
embedTextContent :: FilePath -> Q Exp
embedTextContent filePath =
  liftIO $ Lazy.readFile filePath >>= lift

htmlHandlerDynamic :: FilePath -> Handler FileContent
htmlHandlerDynamic path = do
  html <- liftIO $ Lazy.readFile $ strip path
  return $ FileContent $ html

htmlHandlerEmbedded :: ByteString -> Handler FileContent
htmlHandlerEmbedded = return . FileContent

htmlHandlerSigniture :: String -> Dec
htmlHandlerSigniture functionName = SigD (mkName functionName) (AppT (ConT ''Handler) (ConT ''FileContent))

listStaticFileNames :: FilePath -> IO [String]
listStaticFileNames directoryPath =
  getDirectoryContents directoryPath
    >>= return
      . filter
        ( \fileName ->
            fileName /= "."
              && fileName /= ".."
        )
lstrip :: String -> String
lstrip [] = []
lstrip xs'@(x : xs)
  | x == ' ' = lstrip xs
  | otherwise = xs'

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip
