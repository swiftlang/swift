// RUN: %sourcekitd-test -req=collect-decl-usr %s -- %s -module-name DeclUSR | %FileCheck %s

struct MyStruct {
  let stored: Int

  func method(parameter: Int) -> Int {
    return parameter + stored
  }
}

// CHECK:      key.kind: source.lang.swift.decl.struct
// CHECK-NEXT: key.usr: "s:7DeclUSR8MyStructV"
// CHECK-NEXT: key.offset: 101
// CHECK-NEXT: key.length: 8
// CHECK:      key.kind: source.lang.swift.decl.var.instance
// CHECK-NEXT: key.usr: "s:7DeclUSR8MyStructV6storedSivp"
// CHECK-NEXT: key.offset: 118
// CHECK-NEXT: key.length: 6
// CHECK:      key.kind: source.lang.swift.decl.function.method.instance
// CHECK-NEXT: key.usr: "s:7DeclUSR8MyStructV6method9parameterS2i_tF"
// CHECK-NEXT: key.offset: 138
// CHECK-NEXT: key.length: 6
// CHECK:      key.kind: source.lang.swift.decl.var.parameter
// CHECK-NEXT: key.usr: "s:7DeclUSR8MyStructV6method9parameterS2i_tFAEL_Sivp"
// CHECK-NOT: source.lang.swift.decl.function.constructor
