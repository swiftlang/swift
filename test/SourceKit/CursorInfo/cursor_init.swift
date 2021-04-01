struct A {
  init(arg: Int) {}
  init(arg: Bool) {}
}

func test() {
  _ = A(arg: 1)
  _ = A(arg: true)
}

// RUN: %sourcekitd-test -req=cursor -pos=7:7 %s -- %s | %FileCheck -check-prefix=CHECK-INT %s
// CHECK-INT: source.lang.swift.ref.struct
// CHECK-INT: s:11cursor_init1AV
// CHECK-INT: SECONDARY SYMBOLS BEGIN
// CHECK-INT-NEXT: source.lang.swift.ref.function.constructor {{[^|]*}}
// CHECK-INT-NEXT: init(arg:)
// CHECK-INT-NEXT: s:11cursor_init1AV3argACSi_tcfc
// CHECK-INT-NEXT: source.lang.swift
// CHECK-INT: -----
// CHECK-INT-NEXT: SECONDARY SYMBOLS END

// RUN: %sourcekitd-test -req=cursor -pos=8:7 %s -- %s | %FileCheck -check-prefix=CHECK-BOOL %s
// CHECK-BOOL: source.lang.swift.ref.struct
// CHECK-BOOL: s:11cursor_init1AV
// CHECK-BOOL: SECONDARY SYMBOLS BEGIN
// CHECK-BOOL-NEXT: source.lang.swift.ref.function.constructor {{[^|]*}}
// CHECK-BOOL-NEXT: init(arg:)
// CHECK-BOOL-NEXT: s:11cursor_init1AV3argACSb_tcfc
// CHECK-BOOL-NEXT: source.lang.swift
// CHECK-BOOL: -----
// CHECK-BOOL-NEXT: SECONDARY SYMBOLS END
