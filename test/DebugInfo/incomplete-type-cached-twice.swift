// A nominal type whose fields lack complete type info is emitted as a forward
// declaration. The temporary node that guards against recursion while the
// type is built must not be left behind in the type caches when that happens:
// asking for the same type again used to hit the "type is already cached"
// assertion in createTemporaryReplaceableForwardDecl.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution \
// RUN:   -emit-module-path=%t/resilient_struct.swiftmodule \
// RUN:   -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend %s -I %t -emit-ir -g -gdwarf-types \
// RUN:   -parse-as-library -module-name main -o - | %FileCheck %s

import resilient_struct

// A class with a field of a resilient type has no fixed layout.
final class C {
  var size = Size(w: 1, h: 2)
}

// Each function declares a variable of type C, so the type is requested for
// debug info once per function.
@inline(never) func first(_ c: C) -> Int { c.size.w }
@inline(never) func second(_ c: C) -> Int { c.size.h }

// The incomplete class is emitted exactly once, as a forward declaration, and
// the parameter of both functions refers to it.
// CHECK: ![[C:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s4main1CCD", {{.*}}flags: DIFlagFwdDecl
// CHECK: ![[CONST_C:[0-9]+]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[C]])
// CHECK: ![[FIRST:[0-9]+]] = distinct !DISubprogram(name: "first"
// CHECK: !DILocalVariable(name: "c", arg: 1, scope: ![[FIRST]], {{.*}}type: ![[CONST_C]]
// CHECK: ![[SECOND:[0-9]+]] = distinct !DISubprogram(name: "second"
// CHECK: !DILocalVariable(name: "c", arg: 1, scope: ![[SECOND]], {{.*}}type: ![[CONST_C]]
// CHECK-NOT: name: "$s4main1CCD"
