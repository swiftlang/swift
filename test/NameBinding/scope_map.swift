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

func functionBodies1(a: Int, b: Int?) {
  let (x1, x2) = (a, b),
      (y1, y2) = (b, a)
  let (z1, z2) = (a, a)
  do {
    let a1 = a
    let a2 = a
    do {
      let b1 = b
      let b2 = b
    }
  }
  do {
    let b1 = b
    let b2 = b
  }
  func f(_ i: Int) -> Int { return i }
  let f2 = f(_:)
  struct S7 { }
  typealias S7Alias = S7
  
  if let b1 = b, let b2 = b {
    let c1 = b
  } else {
    let c2 = b
  }

  guard let b1 = b, let b2 = b else {
    let c = 5
    return
  }

  while let b3 = b, let b4 = b {
    let c = 5
  }

  repeat { } while true;

  for (x, y) in [(1, "hello"), (2, "world")] where x % 2 == 0 {

  }

  do {
    try throwing()
  } catch let mine as MyError where mine.value == 17 {
  } catch {
  }

  switch MyEnum.second(1) {
  case .second(let x) where x == 17:
    break;

  case .first:
    break;

  default:
    break;
  }

}

func throwing() throws { }

struct MyError : Error {
  var value: Int
}

enum MyEnum {
  case first
  case second(Int)
  case third
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
// CHECK-EXPANDED-NEXT:   -AbstractFunctionParams {{.*}} deinit param 0:0 [29:3 - 30:3] expanded
// CHECK-EXPANDED-NEXT:     -BraceStmt {{.*}} [29:10 - 30:3] expanded
// CHECK-EXPANDED-NEXT: -GenericParams {{.*}} param 0 [33:25 - 33:32] expanded
// CHECK-EXPANDED-NEXT: {{^[|`]}}-TypeOrExtensionBody {{.*}} '{{.*}}ArchStruct' [{{.*}}] expanded
// CHECK-EXPANDED-NEXT: {{^}}|-AbstractFunctionParams {{.*}} functionBodies1(a:b:) param 0:0 [41:25 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}  `-AbstractFunctionParams {{.*}} functionBodies1(a:b:) param 0:1 [41:36 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-BraceStmt {{.*}} [41:39 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}      `-AfterPatternBinding {{.*}} entry 0 [42:23 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}        `-AfterPatternBinding {{.*}} entry 1 [43:23 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}          `-AfterPatternBinding {{.*}} entry 0 [44:23 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}            |-BraceStmt {{.*}} [45:6 - 52:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}              `-AfterPatternBinding {{.*}} entry 0 [46:14 - 52:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                `-AfterPatternBinding {{.*}} entry 0 [47:14 - 52:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                  `-BraceStmt {{.*}} [48:8 - 51:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}                    `-AfterPatternBinding {{.*}} entry 0 [49:16 - 51:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      `-AfterPatternBinding {{.*}} entry 0 [50:16 - 51:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}            |-BraceStmt {{.*}} [53:6 - 56:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}              `-AfterPatternBinding {{.*}} entry 0 [54:14 - 56:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                `-AfterPatternBinding {{.*}} entry 0 [55:14 - 56:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}            `-LocalDeclaration {{.*}} [57:3 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}              |-AbstractFunctionParams {{.*}} f(_:) param 0:0 [57:15 - 57:38] expanded
// CHECK-EXPANDED-NEXT: {{^}}                `-BraceStmt {{.*}} [57:27 - 57:38] expanded
// CHECK-EXPANDED-NEXT: {{^}}              `-AfterPatternBinding {{.*}} entry 0 [58:16 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}                `-LocalDeclaration {{.*}} [59:3 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}                  |-TypeOrExtensionBody {{.*}} 'S7' [59:13 - 59:15] expanded
// CHECK-EXPANDED-NEXT: {{^}}                  `-LocalDeclaration {{.*}} [60:3 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}                    |-IfStmt {{.*}} [62:3 - 66:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      |-ConditionalClause {{.*}} index 0 [62:18 - 64:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                        `-ConditionalClause {{.*}} index 1 [62:29 - 64:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          `-BraceStmt {{.*}} [62:29 - 64:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            `-AfterPatternBinding {{.*}} entry 0 [63:14 - 64:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      `-BraceStmt {{.*}} [64:10 - 66:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                        `-AfterPatternBinding {{.*}} entry 0 [65:14 - 66:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                    `-GuardStmt {{.*}} [68:3 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      |-ConditionalClause {{.*}} index 0 [68:21 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}                        `-ConditionalClause {{.*}} index 1 [71:3 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          |-ConditionalClause {{.*}} index 0 [73:21 - 75:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            `-ConditionalClause {{.*}} index 1 [73:32 - 75:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [73:32 - 75:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                                `-AfterPatternBinding {{.*}} entry 0 [74:13 - 75:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          |-RepeatWhileStmt {{.*}} [77:3 - 77:20] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            `-BraceStmt {{.*}} [77:10 - 77:12] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          |-ForEachStmt {{.*}} [79:3 - 81:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            `-ForEachPattern {{.*}} [79:52 - 81:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [79:63 - 81:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          |-DoCatchStmt {{.*}} [83:3 - 87:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            |-BraceStmt {{.*}} [83:6 - 85:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            |-CatchStmt {{.*}} [85:31 - 86:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [85:54 - 86:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            `-CatchStmt {{.*}} [86:11 - 87:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [86:11 - 87:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          `-SwitchStmt {{.*}} [89:3 - 98:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            |-CaseStmt {{.*}} [90:29 - 91:10] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [91:5 - 91:10] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            |-CaseStmt {{.*}} [94:5 - 94:10] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [94:5 - 94:10] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            `-CaseStmt {{.*}} [97:5 - 97:10] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [97:5 - 97:10] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      `-BraceStmt {{.*}} [68:37 - 71:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                        `-AfterPatternBinding {{.*}} entry 0 [69:13 - 71:3] expanded
