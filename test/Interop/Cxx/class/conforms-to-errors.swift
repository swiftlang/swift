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

struct  __attribute__((swift_attr("conforms_to:X")))
    __attribute__((swift_attr("conforms_to:X"))) CXX {};
struct  __attribute__((swift_attr("conforms_to:X")))
    __attribute__((swift_attr("conforms_to:Mod.X"))) CXModX {};
struct  __attribute__((swift_attr("conforms_to:X")))
    __attribute__((swift_attr("conforms_to:SwiftTest.X"))) CXTextX {};
struct  __attribute__((swift_attr("conforms_to:X")))
    __attribute__((swift_attr("conforms_to:SwiftTest.A"))) CXA {};
struct  __attribute__((swift_attr("conforms_to:X")))
    __attribute__((swift_attr("conforms_to:SwiftTest.B"))) CXB {};
struct  __attribute__((swift_attr("conforms_to:X")))
    __attribute__((swift_attr("conforms_to:SwiftTest.C"))) CXC {};


struct  __attribute__((swift_attr("conforms_to:Mod.X")))
    __attribute__((swift_attr("conforms_to:Mod.X"))) CModXModX {};
struct  __attribute__((swift_attr("conforms_to:Mod.X")))
    __attribute__((swift_attr("conforms_to:SwiftTest.X"))) CModXTestX {};
struct  __attribute__((swift_attr("conforms_to:Mod.X")))
    __attribute__((swift_attr("conforms_to:SwiftTest.A"))) CModXA {};
struct  __attribute__((swift_attr("conforms_to:Mod.X")))
    __attribute__((swift_attr("conforms_to:SwiftTest.B"))) CModXB {};
struct  __attribute__((swift_attr("conforms_to:Mod.X")))
    __attribute__((swift_attr("conforms_to:SwiftTest.C"))) CModXC {};

struct  __attribute__((swift_attr("conforms_to:SwiftTest.X")))
    __attribute__((swift_attr("conforms_to:SwiftTest.X"))) CTestXTextX {};
struct  __attribute__((swift_attr("conforms_to:SwiftTest.X")))
    __attribute__((swift_attr("conforms_to:SwiftTest.A"))) CTextXA {};
struct  __attribute__((swift_attr("conforms_to:SwiftTest.X")))
    __attribute__((swift_attr("conforms_to:SwiftTest.B"))) CTextXB {};
struct  __attribute__((swift_attr("conforms_to:SwiftTest.X")))
    __attribute__((swift_attr("conforms_to:SwiftTest.C"))) CTextXC {};


struct  __attribute__((swift_attr("conforms_to:SwiftTest.A")))
    __attribute__((swift_attr("conforms_to:SwiftTest.A"))) CAA {};
struct  __attribute__((swift_attr("conforms_to:SwiftTest.A")))
    __attribute__((swift_attr("conforms_to:SwiftTest.B"))) CAB {};
struct  __attribute__((swift_attr("conforms_to:SwiftTest.A")))
    __attribute__((swift_attr("conforms_to:SwiftTest.C"))) CAC {};

struct  __attribute__((swift_attr("conforms_to:SwiftTest.B")))
    __attribute__((swift_attr("conforms_to:SwiftTest.B"))) CBB {};
struct  __attribute__((swift_attr("conforms_to:SwiftTest.B")))
    __attribute__((swift_attr("conforms_to:SwiftTest.C"))) CBC {};

struct  __attribute__((swift_attr("conforms_to:SwiftTest.C")))
    __attribute__((swift_attr("conforms_to:SwiftTest.C"))) CCC {};

struct  __attribute__((swift_attr("conforms_to:SwiftTest.D"))) CD {};
struct  __attribute__((swift_attr("conforms_to:SwiftTest.D"))) CDD: CD {};

//--- test.swift

import Test

struct B {}

protocol A {}
protocol A {}

protocol C {}
protocol D {}

// CHECK: error: expected module name and protocol name separated by '.' in protocol conformance; 'X' is invalid
// CHECK: module 'Mod' in specified protocol conformance 'Mod.X' is not found; did you mean to import it first?
// CHECK: error: protocol 'X' in specified protocol conformance is not found in module 'SwiftTest'
// CHECK: error: ambiguous reference to protocol 'A' in specified protocol conformance; module 'SwiftTest' contains multiple protocols named 'A'
// CHECK: error: struct 'B' referenced in protocol conformance 'SwiftTest.B' is not a protocol
func test(_ inv: CInv, _ invMod: CModInv, _ x: CX, _ a: CA, _ b: CB) {}

// CHECK: error: expected module name and protocol name separated by '.' in protocol conformance; 'X' is invalid
// CHECK: module 'Mod' in specified protocol conformance 'Mod.X' is not found; did you mean to import it first?
// CHECK: error: protocol 'X' in specified protocol conformance is not found in module 'SwiftTest'
// CHECK: error: ambiguous reference to protocol 'A' in specified protocol conformance; module 'SwiftTest' contains multiple protocols named 'A'
// CHECK: error: struct 'B' referenced in protocol conformance 'SwiftTest.B' is not a protocol
func test(_ xx: CXX, _ xModx: CXModX, _ xTextX: CXTextX, _ cxa: CXA, _ cxb: CXB, _ cxc: CXC) {}

// CHECK: module 'Mod' in specified protocol conformance 'Mod.X' is not found; did you mean to import it first?
// CHECK: error: protocol 'X' in specified protocol conformance is not found in module 'SwiftTest'
// CHECK: error: ambiguous reference to protocol 'A' in specified protocol conformance; module 'SwiftTest' contains multiple protocols named 'A'
// CHECK: error: struct 'B' referenced in protocol conformance 'SwiftTest.B' is not a protocol
func test(_ modXModX: CModXModX, _ modXTestX: CModXTestX, _ modXA: CModXA, _ modXB: CModXB, _ modXC: CModXC) {}

// CHECK: error: protocol 'X' in specified protocol conformance is not found in module 'SwiftTest'
// CHECK: error: ambiguous reference to protocol 'A' in specified protocol conformance; module 'SwiftTest' contains multiple protocols named 'A'
// CHECK: error: struct 'B' referenced in protocol conformance 'SwiftTest.B' is not a protocol
func test(_ testXTextX: CTestXTextX, _ textXA: CTextXA, _ textXB: CTextXB, _ textXC: CTextXC) {}

// CHECK: error: ambiguous reference to protocol 'A' in specified protocol conformance; module 'SwiftTest' contains multiple protocols named 'A'
// CHECK: error: struct 'B' referenced in protocol conformance 'SwiftTest.B' is not a protocol
func test(_ aa: CAA, _ ab: CAB, _ ac: CAC)

// CHECK: error: struct 'B' referenced in protocol conformance 'SwiftTest.B' is not a protocol
func test(_ bb: CBB, _ bc: CBC) {}

// CHECK: error: redundant conformance of 'CCC' to protocol 'SwiftTest.C'
// CHECK: error: redundant conformance of 'CDD' to protocol 'SwiftTest.D'
func test(_ cc: CCC, _ dd: CDD) {}
