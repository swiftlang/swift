// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -I=%S/Inputs/custom-modules %s
// RUN: ls -lR %t/clang-module-cache | grep script.pcm

import script // Clang module

var _ : ScriptTy
