{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Trans
import GHC
import GHC.CoreToStg
import GHC.Driver.Config.CoreToStg
import GHC.Driver.Session
import GHC.HsToCore
import GHC.Paths (libdir)
import GHC.Unit.Module.ModGuts
import GHC.Utils.Outputable

showGhc :: (Outputable a) => a -> String
showGhc = showPprUnsafe

banner :: (MonadIO m) => String -> m ()
banner msg =
  liftIO $
    putStrLn
      ( (replicate (fromIntegral n) '=')
          ++ msg
          ++ (replicate (fromIntegral n) '=')
      )
  where
    n = (76 - length msg) `div` 2

main :: IO ()
main = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags {backend = interpreterBackend}

  let moduleName = "Example"
  let filename = moduleName ++ ".hs"

  target <- guessTarget filename Nothing Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName moduleName

  pmod <- parseModule modSum -- ModuleSummary
  tmod <- typecheckModule pmod -- TypecheckedSource
  dmod <- desugarModule tmod -- DesugaredModule
  let core = coreModule dmod -- CoreModule
  let mloc =
        ModLocation
          { ml_hs_file = Just filename,
            ml_hi_file = error "we have no ml_hi_file",
            ml_obj_file = error "we have no ml_obj_file",
            ml_dyn_obj_file = error "we have no ml_obj_file",
            ml_dyn_hi_file = error "we have no ml_dyn_hi_file",
            ml_hie_file = error "we have no ml_hie_file"
          }
  let (stg_binds, denv, cost_centre_info) = coreToStg (initCoreToStgOpts dflags) (mg_module core) mloc (mg_binds core)

  liftIO $ banner "Parsed Source"
  liftIO $ putStrLn $ showGhc (parsedSource pmod)

  liftIO $ banner "Renamed Module"
  liftIO $ putStrLn $ showGhc (tm_renamed_source tmod)

  liftIO $ banner "Typechecked Module"
  liftIO $ putStrLn $ showGhc (tm_typechecked_source tmod)

  liftIO $ banner "Typed Toplevel Definitions"
  liftIO $ putStrLn $ showGhc (modInfoTyThings (moduleInfo tmod))

  liftIO $ banner "Typed Toplevel Exports"
  liftIO $ putStrLn $ showGhc (modInfoExports (moduleInfo tmod))

  liftIO $ banner "Core Module"
  liftIO $ putStrLn $ showGhc (mg_binds core)

   -- TODO: use putDumpFileMaybe logger flag header FormatSTG (pprStgTopBindings ppr_opts stg_binds)
  -- liftIO $ banner "STG"
  -- liftIO $ putStrLn $ showGhc stg_binds
