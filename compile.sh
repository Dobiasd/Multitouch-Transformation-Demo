rm -r build
rm -r cache
elm -m --src-dir=./src --runtime=elm-runtime.js src/MultitouchTransformationDemo.elm
cp $HOME/.cabal/share/Elm-0.10.1/elm-runtime.js ./build
mv ./build/src/MultitouchTransformationDemo.html ./build/index.html
rm -r ./build/src