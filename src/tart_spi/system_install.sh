!/bin/sh
sudo apt install ghc cabal-install
cabal update
cabal install --lib text turtle
