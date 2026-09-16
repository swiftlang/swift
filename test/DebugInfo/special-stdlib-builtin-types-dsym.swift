/*
REQUIRES: OS_FAMILY=darwin

Verify the DIEs for the special stdlib builtin types survive dsymutil's DWARF
linker. Nothing in the program refers to these types, so
DIBuilder::retainType() alone does not keep them: the
DW_TAG_imported_declaration anchor emitted alongside each one is what stops the
DWARFLinker from pruning it. special-stdlib-builtin-types.swift covers the
emission itself, on IR, and so runs everywhere; this test covers survival.

RUN: %empty-directory(%t)
RUN: %target-swift-frontend -c -g -gdwarf-types -o %t/main.o %s
RUN: %target-build-swift -g -o %t/a.out %t/main.o
RUN: %dsymutil %t/a.out -o %t/a.out.dSYM
RUN: %llvm-dwarfdump --debug-info %t/a.out.dSYM | %FileCheck %s

Builtin.UnknownObject ("BO") and Builtin.UnsafeValueBuffer ("BB") are the two a
debugger could not find at all before: the first had no DIE, and the second was
named "<unknown>".

CHECK-DAG: DW_AT_name ("$sBOD")
CHECK-DAG: DW_AT_name ("$sBBD")

CHECK-DAG: DW_AT_name ("$sBoD")
CHECK-DAG: DW_AT_name ("$sBbD")
CHECK-DAG: DW_AT_name ("$sBpD")
CHECK-DAG: DW_AT_name ("$syyXfD")
CHECK-DAG: DW_AT_name ("$sypXpD")

The extra inhabitant counts have to survive too, not just the names -- they are
what the descriptor is for.

CHECK-DAG: DW_AT_LLVM_num_extra_inhabitants

Spot-check that the anchors themselves are in the linked output.

CHECK-DAG: DW_AT_import ({{0x[0-9a-f]+}} "$sBOD")
CHECK-DAG: DW_AT_import ({{0x[0-9a-f]+}} "$sBBD")
*/

func use<T>(_ t: T) {}
use(42)
