/*
RUN: %empty-directory(%t)
RUN: split-file %s %t

Verify the Swift compiler emits DW_TAG_typedef DIEs for the stdlib C-interop
typealiases (CChar, CInt, CDouble, CWideChar, ...) declared in
stdlib/public/core/CTypes.swift, each anchored with a
DW_TAG_imported_declaration so dsymutil's DWARFLinker preserves them across
linking. These aliases can travel into LLDB through clang-imported types'
field metadata or via reflection metadata even when the user's Swift source
never names them directly. Because the clang importer doesn't always
eagerly visit member types of types it imports (e.g.,
`class SwiftChild : ObjCClass {}` never imports `ObjCClass.number`'s type),
we can't drive emission off the importer's visit set; we emit the full set
unconditionally. Bloat is bounded -- ~22 typedef + ~22 anchor DIEs per
binary.

RUN: %target-swift-frontend -emit-ir -g %t/main.swift -I %t -o - | %FileCheck %s
RUN: %target-swift-frontend -emit-ir    %t/main.swift -I %t -o - | %FileCheck %s --check-prefix NDEBUG

Spot-check a representative subset of the aliases. Each typedef DIE has the
mangled-name form of the alias and an underlying type pointing at the
canonical stdlib struct.

Top-level integer aliases.
CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$ss5CCharaD"
CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$ss4CIntaD"
CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$ss5CLongaD"
CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$ss9CLongLongaD"

Floating point aliases.
CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$ss6CFloataD"
CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$ss7CDoubleaD"

Character / boolean aliases.
CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$ss6CChar8aD"
CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$ss7CChar16aD"
CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$ss5CBoolaD"

Aliases of the nested stdlib type Swift.Unicode.Scalar.
CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$ss7CChar32aD"
CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$ss9CWideCharaD"

Each emitted typedef must be paired with a DW_TAG_imported_declaration
anchor; without it dsymutil prunes the typedef during DWARF linking.
(test/DebugInfo/stdlib-c-interop-typealiases-dsym.swift verifies survival.)
CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_declaration,{{.*}} entity: !{{[0-9]+}}

NDEBUG-NOT: DIDerivedType
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
