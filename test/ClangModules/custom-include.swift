// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -constraint-checker -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -I=%S/Inputs/custom-modules %s -parse -verify

import ExternIntX

x += 1
