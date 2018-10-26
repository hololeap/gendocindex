### Generate a documentation index for all Haskell packages installed with Portage

This runs `ghc-pkg dump`, parses the output, and then forms a minimal HTML index for the `haddock-html` locations of each installed pacakge. This makes it easy to view local documentation installed for Haskell packages in Portage using the `doc` USE flag.
