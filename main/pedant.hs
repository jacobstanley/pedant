{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Data (Data)
import qualified Data.Map as Map
import           Data.String (String)
import qualified Data.Text.IO as T

import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Utils
import           Language.Haskell.GHC.ExactPrint.Types

import           P

import           System.IO
import           System.Environment

import           Text.Show.Pretty (ppShow)

-- GHC API --
import           ApiAnnotation
import           HsSyn
import           RdrName
import           SrcLoc


main :: IO ()
main = do
 args <- getArgs
 case args of
  [path] -> do
   msrc <- format path
   case msrc of
     Nothing ->
       pure ()
     Just src ->
       writeFile path src
  _ ->
   T.putStrLn "Usage: pedant <source-path>"

format :: FilePath -> IO (Maybe String)
format path = do
  xx <- parseModule path
  case xx of
    Left (span, err) -> do
      putStrLn $ ppShow span
      putStrLn err
      pure Nothing
    Right (anns0, m0) -> do
      let
        (m, (anns, _), _) =
          runTransform anns0 (formatModule m0)
      pure . Just $
        exactPrint m anns

run :: FilePath -> IO ()
run path = do
  msrc <- format path
  case msrc of
    Nothing ->
      pure ()
    Just src -> do
      putStrLn $ "=== " <> path <> " ==="
      putStr src

--fmt2 :: HsModule RdrName -> Transform (HsModule RdrName)
--fmt2 m = do
--  let
--    go ann =
--      ann { annEntryDelta = goDelta $ annEntryDelta ann }
--
--    goDelta (DP (row, col)) =
--      if row == 1 && col > 0 then
--        DP (1, 2)
--      else
--        DP (row, col)
--
--  modifyAnnsT (fmap go)
--  pure m

dp :: Int -> Int -> DeltaPos
dp row col =
  DP (row, col)

formatModule :: Located (HsModule RdrName) -> Transform (Located (HsModule RdrName))
formatModule m0 = do
  m <- formatDecls m0
  pure m

formatDecls :: HasDecls t => t -> Transform t
formatDecls hd = do
  decls0 <- hsDecls hd
  decls <- traverse formatDecl decls0
  replaceDecls hd decls

formatDecl :: LHsDecl RdrName -> Transform (LHsDecl RdrName)
formatDecl ldecl@(L loc decl0) =
  L loc <$>
  case decl0 of
    TyClD decl -> do
      TyClD <$> formatTyClDecl ldecl decl
    decl -> do
      pure decl

formatTyClDecl :: LHsDecl RdrName -> TyClDecl RdrName -> Transform (TyClDecl RdrName)
formatTyClDecl ld = \case
  DataDecl lname vars defn cusck fvs -> do
    setDPT AnnData ld (dp 0 0)
    setEntryDPT lname (dp 0 1)
    setDPT AnnEqual ld (dp 0 1)
    DataDecl lname
      <$> formatHsQTyVars vars
      <*> formatDataDefn defn
      <*> pure cusck
      <*> pure fvs
  decl -> do
    pure decl

formatHsQTyVars :: LHsQTyVars RdrName -> Transform (LHsQTyVars RdrName)
formatHsQTyVars = \case
  HsQTvs impl expl0 dep -> do
    expl <- traverse formatHsTyVarBndr expl0
    pure $ HsQTvs impl expl dep

formatHsTyVarBndr :: LHsTyVarBndr RdrName -> Transform (LHsTyVarBndr RdrName)
formatHsTyVarBndr (L loc var) =
  L loc <$>
  case var of
    UserTyVar lname -> do
      pure $ UserTyVar lname
    KindedTyVar lname lkind -> do
      pure $ KindedTyVar lname lkind

formatDataDefn :: HsDataDefn RdrName -> Transform (HsDataDefn RdrName)
formatDataDefn = \case
  HsDataDefn nd ctx typ kind cons derivs -> do
    HsDataDefn nd ctx typ kind
      <$> zipWithM formatConDecl [0..] cons
      <*> traverse formatDeriving derivs

formatConDecl :: Int -> LConDecl RdrName -> Transform (LConDecl RdrName)
formatConDecl ix lcon@(L loc con0) =
  L loc <$>
  case con0 of
    ConDeclGADT names typ doc ->
      pure $ ConDeclGADT names typ doc
    ConDeclH98 name qvars ctx details doc -> do
      if ix == 0 then do
        setEntryDPT lcon (dp 1 4)
      else do
        setEntryDPT lcon (dp 0 1)

      setDPT AnnVbar lcon (dp 1 2)

      ConDeclH98 name qvars ctx
        <$> formatConDeclDetails details
        <*> pure doc

formatConDeclDetails ::
  HsConDetails (LBangType RdrName) (Located [LConDeclField RdrName]) ->
  Transform (HsConDetails (LBangType RdrName) (Located [LConDeclField RdrName]))
formatConDeclDetails = \case
  PrefixCon args ->
    PrefixCon <$> traverse formatLBangType args
  details -> do
    pure details

formatLBangType :: LBangType RdrName -> Transform (LBangType RdrName)
formatLBangType lbang = do
  setEntryDPT lbang (dp 0 1)
  pure lbang

formatDeriving :: Located [LHsSigType RdrName] -> Transform (Located [LHsSigType RdrName])
formatDeriving ld = do
  setEntryDPT ld (dp 1 4)
  pure ld

setDPT :: Data a => AnnKeywordId -> Located a -> DeltaPos -> Transform ()
setDPT kw loc pos =
  modifyAnnsT (setDP kw loc pos)

hasDPT :: Data a => AnnKeywordId -> Located a -> Transform Bool
hasDPT kw loc = do
  anns <- getAnnsT
  pure $ hasDP kw loc anns

setDP :: Data a => AnnKeywordId -> Located a -> DeltaPos -> Anns -> Anns
setDP kw0 loc pos =
  mapAnnsDP loc . fmap $ \case
    (G kw, _) | kw == kw0 ->
      (G kw, pos)
    x ->
      x

hasDP :: Data a => AnnKeywordId -> Located a -> Anns -> Bool
hasDP kw ast =
  onAnnsDP ast $
    any (== G kw) . fmap fst

onAnnsDP :: Data a => Located a -> ([(KeywordId, DeltaPos)] -> b) -> Anns -> b
onAnnsDP ast f anns =
  case Map.lookup (mkAnnKey ast) anns of
    Nothing  ->
      f []
    Just ann ->
      f (annsDP ann)

mapAnnsDP :: Data a => Located a -> ([(KeywordId, DeltaPos)] -> [(KeywordId, DeltaPos)]) -> Anns -> Anns
mapAnnsDP ast f anns =
  case Map.lookup (mkAnnKey ast) anns of
    Nothing  ->
      anns
    Just ann0 ->
      let
        ann =
          ann0 { annsDP = f (annsDP ann0) }
      in
        Map.insert (mkAnnKey ast) ann anns

setEntryDPM :: Data a => Maybe (Located a) -> DeltaPos -> Transform ()
setEntryDPM m pos =
  case m of
    Nothing ->
      pure ()
    Just x ->
      setEntryDPT x pos

traceL :: Data a => Located a -> Transform ()
traceL loc = do
  anns <- getAnnsT
  case Map.lookup (mkAnnKey loc) anns of
    Nothing ->
      pure ()
    Just ann ->
      traceM (ppShow ann)
