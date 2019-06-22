@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
  init(initialValue: T) {
    wrappedValue = initialValue
  }
}

struct MyStruct {
  @Wrapper
  var foo: Int = 10
  func doStuff() {
    _ = foo
    _ = $foo
  }
}

// Tests that CursorInfo falls through to the wrapped property on
// property wrapper backing properties occurreces, and that global
// rename is reported as available on both (i.e. from foo and $foo).
//
// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=12:7 %s -- %s | %FileCheck -check-prefixes=CHECK,CHECK_DECL %s
// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=14:9 %s -- %s | %FileCheck -check-prefixes=CHECK,CHECK_REF %s
// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=15:9 %s -- %s | %FileCheck -check-prefixes=CHECK,CHECK_REF %s
// CHECK_DECL: source.lang.swift.decl.var.instance (12:7-12:10)
// CHECK_REF: source.lang.swift.ref.var.instance (12:7-12:10)
// CHECK-NEXT: foo
// CHECK: ACTIONS BEGIN
// CHECK: source.refactoring.kind.rename.global
// CHECK: ACTIONS END
