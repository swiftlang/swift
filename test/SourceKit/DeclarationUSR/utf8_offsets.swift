// RUN: %sourcekitd-test -req=collect-decl-usr %s -- %s | %FileCheck %s

// café
struct ByteOffsetStruct {
  let asciiName: Int
}

// CHECK: key.kind: source.lang.swift.decl.struct
// CHECK-NEXT: key.usr:
// CHECK-SAME: ByteOffsetStruct
// CHECK-NEXT: key.offset: 89
// CHECK-NEXT: key.length: 16
// CHECK: key.kind: source.lang.swift.decl.var.instance
// CHECK-NEXT: key.usr:
// CHECK-SAME: asciiName
// CHECK-NEXT: key.offset: 114
// CHECK-NEXT: key.length: 9
// CHECK-NOT: source.lang.swift.decl.function.constructor
