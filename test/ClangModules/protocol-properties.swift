// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -constraint-checker -emit-llvm -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -I=%S/Inputs/custom-modules %s

import TestProtocols

// This used to crash IRGen because Foo has a property.
var p = FooProto
