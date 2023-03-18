// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop -module-name SwiftTest 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h

struct  __attribute__((swift_attr("conforms_to:X"))) CInv {};
struct  __attribute__((swift_attr("conforms_to:Mod.X"))) CModInv {};
struct  __attribute__((swift_attr("conforms_to:SwiftTest.X"))) CX {};
struct  __attribute__((swift_attr("conforms_to:SwiftTest.A"))) CA {};
struct  __attribute__((swift_attr("conforms_to:SwiftTest.B"))) CB {};

//--- test.swift

import Test

struct B {}

protocol A {}
protocol A {}

// CHECK: error: expected module name and protocol name separated by '.' in protocol conformance; 'X' is invalid
// CHECK: module 'Mod' in specified protocol conformance 'Mod.X' is not found; did you mean to import it first?
// CHECK: error: protocol 'X' in specified protocol conformance is not found in module 'SwiftTest'
// CHECK: error: ambiguous reference to protocol 'A' in specified protocol conformance; module 'SwiftTest' contains multiple protocols named 'A'
// CHECK: error: struct 'B' referenced in protocol conformance 'SwiftTest.B' is not a protocol

func test(_ inv: CInv, _ invMod: CModInv, _ x: CX, _ a: CA, _ b: CB) {}
