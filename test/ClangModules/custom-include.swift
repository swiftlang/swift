// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -I=%S/Inputs/custom-modules %s -parse -verify

import ExternIntX

x += 1
