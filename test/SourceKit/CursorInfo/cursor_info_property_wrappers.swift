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

// Split between custom attr and initializer
extension Wrapper {
    init(initialValue: T, fieldNumber: Int, special: Bool = false) {
        wrappedValue = initialValue
    }
}

let someValue = 10
struct OtherStruct {
    @Wrapper(fieldNumber: someValue, special: true)
    var complex: Int = someValue
}

// Tests that CursorInfo falls through to the wrapped property on
// property wrapper backing properties occurreces, and that global
// rename is reported as available on both (i.e. from foo and $foo).
//
// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=11:7 %s -- %s | %FileCheck -check-prefixes=CHECK,CHECK_DECL %s
// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=13:9 %s -- %s | %FileCheck -check-prefixes=CHECK,CHECK_REF %s
// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=14:9 %s -- %s | %FileCheck -check-prefixes=CHECK,CHECK_REF %s
// CHECK_DECL: source.lang.swift.decl.var.instance (11:7-11:10)
// CHECK_REF: source.lang.swift.ref.var.instance (11:7-11:10)
// CHECK-NEXT: foo
// CHECK: ACTIONS BEGIN
// CHECK: source.refactoring.kind.rename.global
// CHECK: ACTIONS END
//
// Tests that CursofInfo finds occurrences within a property wrapper
// constructor call where the arguments are split across the custom
// attribute argument and the var initializer.
//
// RUN: %sourcekitd-test -req=cursor -pos=25:5 %s -- %s | %FileCheck -check-prefixes=CHECK2,CHECK2_DECL %s
// RUN: %sourcekitd-test -req=cursor -pos=27:27 %s -- %s | %FileCheck -check-prefixes=CHECK2,CHECK2_REF %s
// RUN: %sourcekitd-test -req=cursor -pos=28:24 %s -- %s | %FileCheck -check-prefixes=CHECK2,CHECK2_REF %s
// CHECK2_DECL: source.lang.swift.decl.var.global (25:5-25:14)
// CHECK2_REF: source.lang.swift.ref.var.global (25:5-25:14)
// CHECK2-NEXT: someValue
