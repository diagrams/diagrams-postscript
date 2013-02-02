[![Build Status](https://secure.travis-ci.org/diagrams/diagrams-postscript.png)](http://travis-ci.org/diagrams/diagrams-postscript)

_diagrams-postscript_ is a rendering backend for [diagrams], a powerful,
flexible, declarative domain-specific language for creating vector graphics,
using the [Haskell programming language][haskell].

[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell

_diagrams-postscript_ has a simple rendering engine that outputs
[postscript] files and is an officially supported backend
for diagrams.

[postscript]: http://www.adobe.com/products/postscript/pdfs/PLRM.pdf (PostScript™ language)

# Installation

```
cabal update && cabal install diagrams-postscript
```

# Basic usage

A simple example that uses _diagrams-postscript_ to draw a blue circle:

```haskell
import Diagrams.Prelude
import Diagrams.Backend.Postscript.CmdLine

d = circle 1 # fc blue

main = defaultMain (pad 1.1 d)
```

Save this to file named `Circle.hs` and compile it:

```
ghc --make Circle.hs
```

This will generate an executable which, when run, outputs a blue
circle to some file. Run the executable with the `--help` option to
find out more about how to call it.

```
$ ./Circle --help
Command-line diagram generation.

Circle [OPTIONS]

Common flags:
  -w --width=INT    Desired width of the output image
  -h --height=INT   Desired height of the output image
  -o --output=FILE  Output file
  -? --help         Display help message
  -V --version      Print version information
```

The output type will be automatically determined from the file
extension.  Currently EPS and PS are supported.

```
$ ./Circle -o circle.eps -w 400
```

The command above generates an EPS file with a width of 400pt.

# Advanced usage

Instead of just creating a standalone executable, the postscript backend
can also be called from within a larger program.  For more
information, see the Diagram.Backend.Postscript module.
