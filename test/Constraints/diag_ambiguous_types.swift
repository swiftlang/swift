// RUN: not %target-swift-frontend -typecheck %s 2>&1 | %FileCheck -check-prefix CHECK-NO-SHADOWING -check-prefix CHECK_ALL %s
// RUN: not %target-swift-frontend -DSTRING_SHADOWED -typecheck %s 2>&1 | %FileCheck -check-prefix CHECK-STRING-SHADOWED -check-prefix CHECK_ALL %s

#if STRING_SHADOWED
struct String {}
#endif

let any: Any = ""

let x: Swift.String = any
// CHECK-NO-SHADOWING: cannot convert value of type 'Any' to specified type 'String'
// CHECK-NO-SHADOWING: as! String
// CHECK-STRING-SHADOWED: cannot convert value of type 'Any' to specified type 'String'
// CHECK-STRING-SHADOWED: as! Swift.String

let y: [Swift.String] = any
// CHECK-NO-SHADOWING: cannot convert value of type 'Any' to specified type '[String]'
// CHECK-NO-SHADOWING: as! [String]
// CHECK-STRING-SHADOWED: cannot convert value of type 'Any' to specified type '[String]'
// CHECK-STRING-SHADOWED: as! [Swift.String]

let z: ((Swift.String) -> String) = any
// CHECK-NO-SHADOWING: cannot convert value of type 'Any' to specified type '(String) -> String'
// CHECK-NO-SHADOWING: as! (String) -> String
// CHECK-STRING-SHADOWED: cannot convert value of type 'Any' to specified type '(String) -> String'
// CHECK-STRING-SHADOWED: as! (Swift.String) -> String

struct Foo {}

struct Bar {
  struct Foo {}
  
  let baz: (diag_ambiguous_types.Foo, Int, Int) = any
  // CHECK-ALL: cannot convert value of type 'Any' to specified type '(Foo, Int, Int)'
  // CHECK-ALL: as! (diag_ambiguous_types.Foo, Int, Int)
}
