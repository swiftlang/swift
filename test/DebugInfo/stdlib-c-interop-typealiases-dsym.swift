/*
REQUIRES: OS_FAMILY=darwin

Verify the DW_TAG_typedef DIEs for stdlib C-interop typealiases survive
dsymutil's DWARF linker: these are Swift typealiases that may appear in
imported AST nodes without ever being referred to in the actual Swift
source code. We need to make sure to:
 1. Emit debug info for these typedefs unconditionally for every binary.
    This is what test/DebugInfo/stdlib-c-interop-typealiases.swift tests.
    It has the advantage of being cross-platform since it operates on IR.
 2. Make sure that the DW_TAG_imported_declaration successfully anchors
    the DW_AT_name in the binary so that dsymutil doesn't remove it during
    linking. This is what this test tests.

RUN: %empty-directory(%t)
RUN: split-file %s %t

RUN: %target-swift-frontend -c -g -I %t -o %t/main.o %t/main.swift
RUN: %target-build-swift -g -o %t/a.out %t/main.o
RUN: %dsymutil %t/a.out -o %t/a.out.dSYM
RUN: %llvm-dwarfdump --debug-info %t/a.out.dSYM | %FileCheck %s

Spot-check that representative typedef DIEs survived dsymutil. CInt and
CDouble are referenced via interop in this test (StructWithInt.counter and
StructWithDouble.value); CChar and CBool are not referenced at all, but
the compiler currently emits anchors for the full alias set unconditionally,
so they are also present in the final dSYM.

CHECK-DAG: DW_TAG_typedef
CHECK-DAG: DW_AT_name ("$ss4CIntaD")
CHECK-DAG: DW_TAG_typedef
CHECK-DAG: DW_AT_name ("$ss7CDoubleaD")
CHECK-DAG: DW_TAG_typedef
CHECK-DAG: DW_AT_name ("$ss5CCharaD")
CHECK-DAG: DW_TAG_typedef
CHECK-DAG: DW_AT_name ("$ss5CBoolaD")

CHECK-DAG: DW_TAG_imported_declaration
CHECK-DAG: DW_AT_import ({{0x[0-9a-f]+}} "$ss4CIntaD")
CHECK-DAG: DW_TAG_imported_declaration
CHECK-DAG: DW_AT_import ({{0x[0-9a-f]+}} "$ss7CDoubleaD")
*/

//--- main.swift
import CInteropAliasModule

func use<T>(_ t: T) {}
let withInt = StructWithInt(counter: 1)
let withDouble = StructWithDouble(value: 2)
use(withInt)
use(withDouble)

//--- module.modulemap
module CInteropAliasModule {
  header "cinterop-aliases.h"
  export *
}

//--- cinterop-aliases.h
struct StructWithInt {
  int counter;
};

struct StructWithDouble {
  double value;
};
