#!/usr/bin/env bash
rm -r build
mkdir build
elm-make src/MultitouchTransformationDemo.elm --output build/index.html