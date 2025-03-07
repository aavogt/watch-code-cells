import Control.Concurrent hiding (yield)
import Control.Exception
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Char8 as B8
import Data.Foldable
import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Text as T
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit (HelpFormat (..), helpText)
import System.Exit (exitFailure)
import System.FilePath
import System.INotify (Event (..), EventVariety (..), addWatch, initINotify, removeWatch)
import System.IO
import System.IO.Error
import System.Process
import Text.Printf (printf)

data WatchCodeCells = WatchCodeCells
  { filepath :: FilePath,
    reload :: [FilePath],
    restart :: [FilePath],
    test, setup :: String,
    retry_us, retry_attempts :: Int
  }
  deriving (Show, Data)

options :: WatchCodeCells
options =
  WatchCodeCells
    { filepath = def &= args &= typFile,
      reload = def &= typFile &= help "Resend all chunks when the given file or directory changes (defaults to none)",
      restart = def &= typFile &= help "Restart python/R/maxima/julia when the given file or directory changes (defaults to none)",
      test = def &= help "Evaluate this expression after the last cell",
      setup = def &= help "Evaluate this expression before the first cell",
      retry_us = 100000 &= help "When reading filepath, wait this many microseconds",
      retry_attempts = 10 &= help "If reading the filepath fails, try this many attempts. 0 for infinite retries."
    }
    &= summary "watch-code-cells <file>"
    &= details
      [ "Given a file with chunks delimited by `# %%` (.R .py .jl), or `/* [wxMaxima: input   start ] */` (.wxm .mac),",
        "Start the interpreter (R, python, julia, maxima), and send the whole file.",
        "When the file changes, only send the changed chunk and all subsequent chunks.",
        "For reproducible results, a chunk should not write variables it depends on."
      ]

splitContent = splitOn . getDelimiter
  where
    getDelimiter = \case
      ".R" -> "\n# %%"
      ".py" -> "\n# %%"
      p | p `elem` [".mac", ".wxm"] -> "\n/* [wxMaxima: input   start ] */"
      ".Rmd" -> "\n```" -- TODO regex-applicative would be better, plus this format allows different interpreters, chunk options etc.
      ".jl" -> "\n# %%"
      _ -> ""

interpreterName = \case
  ".R" -> ("R", ["-q", "--no-save"])
  ".py" -> ("python", ["-i", "-u"])
  ".mac" -> ("maxima", ["-q"])
  ".jl" -> ("julia", ["-q"])
  x -> error $ "Don't know how to interpret file extension " <> show x

interpreterWrap = \cases
  ".R" x -> "tryCatch({" <> x <> "}, error = function(e) print(e))"
  _ x -> x

stripCommonPrefix :: (Eq a) => [a] -> [a] -> [a]
stripCommonPrefix (x : xs) (y : ys) | x == y = stripCommonPrefix xs ys
stripCommonPrefix _ ys = ys

-- also have an R option
pyprocess ext = do
  hPutStrLn stderr $ "Starting interpreter " <> show (interpreterName ext)
  (Just pyin, _, _, pyh) <-
    createProcess
      (uncurry System.Process.proc (interpreterName ext))
        { std_in = CreatePipe,
          std_out = Inherit,
          std_err = Inherit
        }
  return (pyin, pyh)

main = do
  let options' = cmdArgsMode options
  options@WatchCodeCells {..} <- cmdArgsRun options'
  let ext = takeExtension filepath
  when (null filepath) $ do
    print (helpText [] HelpFormatDefault options')
    exitFailure

  (ch, _inotify) <- setupInotify options
  writeChan ch Chunk -- initial run
  -- variables for the interpreter are py though it could be maxima/R etc.
  pyhv <- newEmptyMVar
  forkIO $ forever do
    l <- getLine
    pyh <- readMVar pyhv
    hPutStrLn pyh l

  forever do
    (pyin, pyh) <- pyprocess ext
    putMVar pyhv pyin

    th <- forkIO $ forever do
      l <- getLine
      pyin <- takeMVar pyhv
      hPutStrLn pyin l
      putMVar pyhv pyin -- exceptions...
      hFlush pyin

    ref <- newIORef []

    let step = do
          chanAct <- readChan ch
          let additions x = setup : x <> [test]
          nc <- additions . splitContent ext <$> readFileRetry retry_us (retry_attempts - 1) filepath
          oc <- readIORef ref
          pyin <- takeMVar pyhv
          hPutStrLn pyin ""
          chunks <- case chanAct of
            Chunk -> return $ stripCommonPrefix oc nc
            Reload -> return nc
            Restart -> throwIO Restart
          for_ chunks (hPutStrLn pyin . interpreterWrap ext)
          putMVar pyhv pyin
          hFlush pyin
          writeIORef ref nc

    let kill = do
          terminateProcess pyh
          hClose pyin
          tryTakeMVar pyhv
          killThread th

    forever step `finally` kill

-- | @readFileRetry us n filepath@ is @readFile filepath@,
-- except if the file does not exist yet it waits @us@,
-- before trying again (@n@ times). Negative n lead to infinite retries.
readFileRetry us n filepath = go n
  where
    go 0 = throwIO (userError "File not found, --retry-attempts or --retry-us may be too low.")
    go n =
      readFile filepath
        `catch` \e -> do
          if isDoesNotExistError e
            then do
              threadDelay us
              go (max (-1) (n - 1))
            else throwIO e

data ChanAct = Chunk | Restart | Reload deriving (Show)

instance Exception ChanAct

-- | @(ch, inotify) <- setupInotify ch opts@ sets up inotify watches such that
-- changes to the files named in the opts.reload, opts.restart, opts.filepath lead to
-- @restart_ <- readChan ch@
setupInotify WatchCodeCells {..} = do
  ch <- newChan
  let inotifyMap =
        M.fromListWith
          (<>)
          [ ( d,
              \ev -> sequenceA_ do
                q <- getMaybeFilePath ev
                guard (p == q)
                Just (writeChan ch doAll)
            )
            | ((d, p), doAll) <-
                (filepath, Chunk) : map (,Restart) restart <> map (,Reload) reload
                  <&> _1 %~ over each B8.pack . splitFileName
          ]
  inotify <- initINotify
  let eventVarieties = [Modify, Attrib, CloseWrite, MoveIn, MoveOut, MoveSelf]
  for_ (M.toList inotifyMap) (uncurry (addWatch inotify eventVarieties))
  return (ch, inotify)

getMaybeFilePath = \case
  Modified {maybeFilePath = x} -> x
  Attributes {maybeFilePath = x} -> x
  _ -> Nothing
