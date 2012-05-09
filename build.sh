echo "Install gtk first, or you will probably have a hard time installing this"
cabal install

mkdir game-dist
cp dist/build/game/game game-dist/

