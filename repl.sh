#!/bin/bash

docker run -it --rm \
  -v "$(pwd)":/usr/src/haskell \
  -w /usr/src/haskell \
  haskell:8
