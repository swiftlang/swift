// Note: test of the scope map. All of these tests are line- and
// column-sensitive, so any additions should go at the end.

struct S0 {
  class InnerC0 { }
}

extension S0 {
}

class C0 {
}

enum E0 {
  case C0
  case C1(Int, Int)
}

struct GenericS0<T, U> {
}

func genericFunc0<T, U>(t: T, u: U, i: Int = 10) {
}

class ContainsGenerics0 {
  init<T, U>(t: T, u: U) {
  }

  deinit {
  }
}

typealias GenericAlias0<T> = [T]

#if arch(i386)
struct i386ArchStruct { }
#else
struct OtherArchStruct { }
#endif

func functionBodies(a: Int, b: Int?) {
  let (x, y) = (a, b),
      (z1, z2) = (b, a)
}

// RUN: %target-swift-frontend -dump-scope-maps expanded %s 2> %t.expanded
// RUN: %FileCheck -check-prefix CHECK-EXPANDED %s < %t.expanded


// CHECK-EXPANDED: SourceFile{{.*}}scope_map.swift{{.*}}expanded
// CHECK-EXPANDED-NEXT: TypeOrExtensionBody {{.*}} 'S0' [4:11 - 6:1] expanded
// CHECK-EXPANDED-NEXT: `-TypeOrExtensionBody {{.*}} 'InnerC0' [5:17 - 5:19] expanded
// CHECK-EXPANDED-NEXT: -TypeOrExtensionBody {{.*}} extension of 'S0' [8:14 - 9:1] expanded
// CHECK-EXPANDED-NEXT: |-TypeOrExtensionBody {{.*}} 'C0' [11:10 - 12:1] expanded
// CHECK-EXPANDED-NEXT: -TypeOrExtensionBody {{.*}} 'E0' [14:9 - 17:1] expanded
// CHECK-EXPANDED-NEXT: -GenericParams {{.*}} param 0 [19:18 - 20:1] expanded
// CHECK-EXPANDED-NEXT:   -GenericParams {{.*}} param 1 [19:21 - 20:1] expanded
// CHECK-EXPANDED-NEXT:     -TypeOrExtensionBody {{.*}} 'GenericS0' [19:24 - 20:1] expanded
// CHECK-EXPANDED-NEXT:  -GenericParams {{.*}} param 0 [22:19 - 23:1] expanded
// CHECK-EXPANDED-NEXT:   -GenericParams {{.*}} param 1 [22:22 - 23:1] expanded
// CHECK-EXPANDED-NEXT:   -AbstractFunctionParams {{.*}} genericFunc0(t:u:i:) param 0:0 [22:28 - 23:1] expanded
// CHECK-EXPANDED-NEXT:     -AbstractFunctionParams {{.*}} genericFunc0(t:u:i:) param 0:1 [22:34 - 23:1] expanded
// CHECK-EXPANDED-NEXT:       -AbstractFunctionParams {{.*}} genericFunc0(t:u:i:) param 0:2 [22:46 - 23:1] expanded
// CHECK-EXPANDED-NEXT:         -BraceStmt {{.*}} [22:50 - 23:1] expanded
// CHECK-EXPANDED-NEXT: -TypeOrExtensionBody {{.*}} 'ContainsGenerics0' [25:25 - 31:1] expanded
// CHECK-EXPANDED-NEXT:   -GenericParams {{.*}} param 0 [26:8 - 27:3] expanded
// CHECK-EXPANDED-NEXT:     -GenericParams {{.*}} param 1 [26:11 - 27:3] expanded
// CHECK-EXPANDED-NEXT:     -AbstractFunctionParams {{.*}} init(t:u:) param 0:0 [26:13 - 27:3] expanded
// CHECK-EXPANDED-NEXT:       -AbstractFunctionParams {{.*}} init(t:u:) param 1:0 [26:17 - 27:3] expanded
// CHECK-EXPANDED-NEXT:         -AbstractFunctionParams {{.*}} init(t:u:) param 1:1 [26:23 - 27:3] expanded
// CHECK-EXPANDED-NEXT:           -BraceStmt {{.*}} [26:26 - 27:3] expanded
// CHECK-EXPANDED-NEXT:             -BraceStmtElement {{.*}} element 0 [27:3 - 27:3] expanded
// CHECK-EXPANDED-NEXT:   -AbstractFunctionParams {{.*}} deinit param 0:0 [29:3 - 30:3] expanded
// CHECK-EXPANDED-NEXT:     -BraceStmt {{.*}} [29:10 - 30:3] expanded
// CHECK-EXPANDED-NEXT: -GenericParams {{.*}} param 0 [33:25 - 33:32] expanded
// CHECK-EXPANDED-NEXT: {{^[|`]}}-TypeOrExtensionBody {{.*}} '{{.*}}ArchStruct' [{{.*}}] expanded
// CHECK-EXPANDED-NEXT: -AbstractFunctionParams {{.*}} functionBodies(a:b:) param 0:0 [41:24 - 44:1] expanded
// CHECK-EXPANDED-NEXT:   `-AbstractFunctionParams {{.*}} functionBodies(a:b:) param 0:1 [41:35 - 44:1] expanded
// CHECK-EXPANDED-NEXT:     `-BraceStmt {{.*}} [41:38 - 44:1] expanded
// CHECK-EXPANDED-NEXT:       `-BraceStmtElement {{.*}} element 0 [42:3 - 44:1] expanded
// CHECK-EXPANDED-NEXT:         `-AfterPatternBinding {{.*}} entry 0 [42:21 - 44:1] expanded
// CHECK-EXPANDED-NEXT:           `-AfterPatternBinding {{.*}} entry 1 [43:23 - 44:1] expanded
