// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -I=%S/Inputs/custom-modules %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// CHECK: script{{.*}}.pcm

import script // Clang module

var _ : ScriptTy
