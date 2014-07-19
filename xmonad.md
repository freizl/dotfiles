# OSX 10.9

- make a final version after install `xmonad` lib and `osxmonad` lib

`LIBRARY_PATH=/usr/X11/lib:$LIBRARY_PATH ghc -v --make -framework=Cocoa -package-db=/Users/haiswu/Downloads/osxmonad/.cabal-sandbox/x86_64-osx-ghc-7.8.3-packages.conf.d Main.hs  -o xmonad`
