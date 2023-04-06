func testAmbiguousFunctionReference() {
  func foo(a: Int) {}
  func foo(a: String) {}

  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):7 %s -- %s | %FileCheck %s
  _ = foo

  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):7 %s -- %s | %FileCheck %s
  _ = foo(a: UInt(1))
}

// CHECK: source.lang.swift.ref.function.free (2:8-2:19)
// CHECK: <Declaration>func foo(a: <Type usr="s:Si">Int</Type>)</Declaration>
// CHECK: SECONDARY SYMBOLS BEGIN
// CHECK: source.lang.swift.ref.function.free (3:8-3:22)
// CHECK: <Declaration>func foo(a: <Type usr="s:SS">String</Type>)</Declaration>
// CHECK: SECONDARY SYMBOLS END
