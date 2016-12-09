// Note: test of the scope map. All of these tests are line- and
// column-sensitive, so any additions should go at the end.

struct S0 { }

let a: Int? = 1

guard let b = a else {
}

func foo() {} // to interrupt the TopLevelCodeDecl

let c = b

typealias T = Int

extension Int {
  func my_identity() -> Int { return self }
}

var i: Int = b.my_identity()


// RUN: %target-swift-frontend -dump-scope-maps expanded %s 2> %t.expanded
// RUN: %FileCheck -check-prefix CHECK-EXPANDED %s < %t.expanded

// CHECK-EXPANDED:      |-TypeDecl {{.*}} S0 [4:1 - 4:13] expanded
// CHECK-EXPANDED-NEXT:   `-TypeOrExtensionBody {{.*}} 'S0' [4:11 - 4:13] expanded
// CHECK-EXPANDED-NEXT: `-TopLevelCode {{.*}} [6:1 - [[EOF:[0-9]+:[0-9]+]]] expanded
// CHECK-EXPANDED:     `-PatternBinding {{.*}} entry 0 [6:5 - [[EOF]]] expanded
// CHECK-EXPANDED-NEXT:       |-PatternInitializer {{.*}} entry 0 [6:15 - 6:15] expanded
// CHECK-EXPANDED-NEXT:       `-AfterPatternBinding {{.*}} entry 0 [6:15 - [[EOF]]] expanded
// CHECK-EXPANDED-NEXT:         `-TopLevelCode {{.*}} [8:1 - [[EOF]]] expanded
// CHECK-EXPANDED: `-ConditionalClause {{.*}} index 0 guard-continuation [9:1 - [[EOF]]] expanded
// CHECK-EXPANDED-NEXT:                 |-AbstractFunctionDecl {{.*}} foo() [11:1 - 11:13] expanded
// CHECK-EXPANDED: `-AfterPatternBinding {{.*}} entry 0 [13:9 - [[EOF]]] expanded
// CHECK-EXPANDED-NEXT:                           |-TypeDecl {{.*}} T [15:1 - 15:15] expanded
