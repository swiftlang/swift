// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | grep objc.pcm

import objc

func treatBAsA(b : B) -> A { 
  return b
}
