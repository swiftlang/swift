// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h
struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:immortal")))
    __attribute__((swift_attr("release:immortal")))
HasCtor {
  HasCtor(int a) {}
};

//--- test.swift

import Test

// CHECK: error: 'HasCtor' cannot be constructed because it has no accessible initializers
let x = HasCtor(42)