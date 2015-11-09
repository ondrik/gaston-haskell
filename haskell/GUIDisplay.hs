module GUIDisplay where

import Control.Monad.IO.Class
import Data.Char
import Graphics.UI.Gtk
import System.Environment
import System.Exit
import System.IO
import System.Directory
import System.Process
import Text.Regex.Posix
import Data.IORef

import qualified Logic
import Logic (Formula, formulaToTex)


tmpTexFileName = "pokus.tex"
tmpDviFileName = "pokus.dvi"
tmpPngFileName = "pokus1.png"

sanitizeFilePath :: FilePath -> IO FilePath
sanitizeFilePath filename = do
  curDir <- getCurrentDirectory
  return $ curDir ++ "/" ++ filename


runLatexOn :: String -> IO ()
runLatexOn filename = do
  eCode <- system $ "latex " ++ filename
  case eCode of
    ExitFailure num ->
      error $ "Error processing LaTeX (exit code " ++ (show num) ++ ")"
    ExitSuccess -> return ()


runDviPngOn :: String -> String -> IO ()
runDviPngOn inFilename outFilename = do
  eCode <- system $ "dvipng " ++ inFilename ++ " -D 200" ++ " -o " ++ outFilename
  case eCode of
    ExitFailure num ->
      error $ "Error processing DviPng (exit code " ++ (show num) ++ ")"
    ExitSuccess -> return ()


-- displays a formula in a window
displayFormula :: Formula -> IO ()
displayFormula phi = do
  curDir <- getCurrentDirectory
  texFilename <- sanitizeFilePath $ tmpTexFileName
  dviFilename <- sanitizeFilePath $ tmpDviFileName
  pngFilename <- sanitizeFilePath $ tmpPngFileName
  writeFile texFilename $ createTex $ formulaToTex phi
  runLatexOn texFilename
  runDviPngOn dviFilename pngFilename
  displayImage pngFilename


-- creates a TeX string with the given content
createTex :: String -> String
createTex content = unlines
  [ "\\documentclass{minimal}"
  , "\\usepackage{amsmath}"
  -- , "\\setlength{\\mathindent}{0pt}"
  , "\\begin{document}"
  , "\\begin{flalign*}"
  , content ++ " && \\\\"
  , "\\end{flalign*}"
  , "\\end{document}"
  ]


-- shows an image in a window
displayImage :: FilePath -> IO ()
displayImage imagePath = do
  initGUI
  window <- windowNew
  window `on` deleteEvent $ liftIO mainQuit >> return False
  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  vbox <- vBoxNew False 0
  hbox <- hBoxNew False 0
  set window [windowDefaultWidth := 900, windowDefaultHeight := 300,
              containerChild := vbox, containerBorderWidth := 0]
  image <- imageNew
  boxPackEnd vbox hbox PackNatural 0
  boxPackStart vbox scroll PackGrow 0
  scrolledWindowAddWithViewport scroll image
  curDir <- getCurrentDirectory
  imageSetFromFile image imagePath
  position <- newIORef 0
  widgetShowAll window
  mainGUI
