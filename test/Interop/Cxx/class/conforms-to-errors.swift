// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h

struct  __attribute__((swift_attr("conforms_to:X"))) CX {};
struct  __attribute__((swift_attr("conforms_to:A"))) CA {};
struct  __attribute__((swift_attr("conforms_to:B"))) CB {};

//--- test.swift

import Test

struct B {}

protocol A {}
protocol A {}

// CHECK: error: specified protocol conformance could not be found 'X'.
// CHECK: error: specified protocol conformance is ambiguous. Found multiple protocols named 'A'.
// CHECK: error: specified protocol conformance 'B' is not a protocol.

func test(_ x: CX, _ a: CA, _ b: CB) {}