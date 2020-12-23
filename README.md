### Generate a documentation index for all Haskell packages

This runs `ghc-pkg dump`, parses the output, and then forms a minimal HTML
index for the `haddock-html` locations of each installed pacakge. The intended
use is to make it easy to view local documentation that is installed using the
`doc` USE flag in Portage, which is scattered around in different directories.

Right now, it has no command line arguments, it just dumps the HTML directly to
stdout, so redirect its output into a file.
