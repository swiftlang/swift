// RUN: %sourcekitd-test -req=collect-decl-usr %s -- %s %S/Inputs/multifile_other.swift | %FileCheck %s

struct PrimaryFileDecl {}

// CHECK-NOT: OtherFileDecl
// CHECK: key.kind: source.lang.swift.decl.struct
// CHECK-NEXT: key.usr:
// CHECK-SAME: PrimaryFileDecl
// CHECK-NEXT: key.offset: 112
// CHECK-NEXT: key.length: 15
// CHECK-NOT: OtherFileDecl
