// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop -disable-availability-checking

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

let x = HasCtor(42)
