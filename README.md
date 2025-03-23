# watch-code-cells

```bash
watch-code-cells [OPTIONS] [FILE]

Common flags:
     --reload=FILE         Resend all chunks when the given file or directory
                           changes (defaults to none)
     --restart=ITEM        Restart python/R/maxima/julia when the given file
                           or directory changes (defaults to none)
  -t --test=ITEM           Evaluate this expression after the last cell
  -s --setup=ITEM          Evaluate this expression before the first cell
  -d --debounce-s=NUM      After receiving an event, delay running this many
                           seconds (0.2 by default): only resend run once this
                           time limit
     --retry-us=INT        When rereading a filepath, wait this many microseconds
     --retry-attempts=INT  If reading the filepath fails, try this many
                           attempts. 0 for infinite retries.
  -? --help                Display help message
  -V --version             Print version information

Given a file with chunks delimited by `# %%` (.R .py .jl), or `/* [wxMaxima:
input   start ] */` (.wxm .mac),
Start the interpreter (R, python, julia, maxima), and send the whole file.
When the file changes, only send the changed chunk and all subsequent chunks.
For reproducible results, a chunk should not write variables it depends on.
```

## installation

At the moment it is linux-only as it uses hinotify, instead of fsnotify.

Get ghc9.6.6 and cabal-install, say with [ghcup](https://www.haskell.org/ghcup/install/)

```bash
git clone https://github.com/aavogt/watch-code-cells.git
cd watch-code-cells
cabal install
```

see also https://github.com/ndmitchell/ghcid, knitr, jupyter

## TODO

 - [ ] additional/configurable events
 - [ ] --auto infer inputs files needs:
  - [x] own pid System.Posix.Process.getProcessID
  - [x] /proc/<own pid>/task/<own pid>/children has the interpreter pid
  - [ ] /proc/<child>/fd/* is the list of open files?
  - [ ] but these change over time?
