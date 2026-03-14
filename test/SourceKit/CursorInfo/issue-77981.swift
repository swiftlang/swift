struct S {
  static func foo(_ x: Int) -> S {}
  static func foo(_ x: String) -> S {}
}

// https://github.com/swiftlang/swift/issues/77981 - Make sure we can resolve
// solver-based cursor info for UnresolvedMemberExprs.
func bar() {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):15 %s -- %s | %FileCheck %s
  let _: S = .foo()
}

// CHECK-DAG: source.lang.swift.ref.function.method.static (2:15-2:28)
// CHECK-DAG: source.lang.swift.ref.function.method.static (3:15-3:31)
