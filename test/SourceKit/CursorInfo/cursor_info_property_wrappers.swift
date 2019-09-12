@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
  init(wrappedValue initialValue: T) {
    wrappedValue = initialValue
  }
  var projectedValue: Projection<T> {
    get { Projection(item: wrappedValue) }
  }
}

struct MyStruct {
  /// Here is some documentation.
  @Wrapper
  var foo: Int = 10
  func doStuff() {
    _ = foo
    _ = _foo
    _ = $foo
  }
}

struct Projection<T> {
  var item: T
}

// Split between custom attr and initializer
extension Wrapper {
    init(wrappedValue initialValue: T, fieldNumber: Int, special: Bool = false) {
        wrappedValue = initialValue
    }
}

let someValue = 10
struct OtherStruct {
    @Wrapper(fieldNumber: someValue, special: true)
    var complex: Int = someValue
}

// Tests we get the same USR and documentation for the foo, _foo and $foo, so
// that rename renames them all at the same time.
//
// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=17:9 %s -- %s | %FileCheck -check-prefixes=CHECK,CHECK_WRAPPED %s
// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=18:9 %s -- %s | %FileCheck -check-prefixes=CHECK,CHECK_BACKING %s
// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=19:9 %s -- %s | %FileCheck -check-prefixes=CHECK,CHECK_PROJECTED %s
//
// CHECK: source.lang.swift.ref.var.instance (15:7-15:10)
// CHECK_PROJECTED: $foo
// CHECK_BACKING: _foo
// CHECK_WRAPPED: foo
// CHECK: s:29cursor_info_property_wrappers8MyStructV3fooSivp
// CHECK_PROJECTED: Projection<Int>
// CHECK_BACKING: Wrapper<Int>
// CHECK_WRAPPED: Int
// CHECK: <CommentParts><Abstract><Para>Here is some documentation.</Para></Abstract></CommentParts>
// CHECK: ACTIONS BEGIN
// CHECK: source.refactoring.kind.rename.global
//
//
// Tests that CursorInfo resolves occurrences within a property wrapper
// constructor call where the arguments are split across the custom attribute
// argument and the var initializer.
//
// RUN: %sourcekitd-test -req=cursor -pos=34:5 %s -- %s | %FileCheck -check-prefixes=CHECK2,CHECK2_DECL %s
// RUN: %sourcekitd-test -req=cursor -pos=36:27 %s -- %s | %FileCheck -check-prefixes=CHECK2,CHECK2_REF %s
// RUN: %sourcekitd-test -req=cursor -pos=37:24 %s -- %s | %FileCheck -check-prefixes=CHECK2,CHECK2_REF %s
// CHECK2_DECL: source.lang.swift.decl.var.global (34:5-34:14)
// CHECK2_REF: source.lang.swift.ref.var.global (34:5-34:14)
// CHECK2-NEXT: someValue
