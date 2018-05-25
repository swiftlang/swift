func foo() -> Int{
  var aaa = 1 + 2
  aaa = aaa + 3
  if aaa == 3 { aaa = 4 }
  return aaa
}

func foo1(a : Int) -> Int {
  switch a {
  case 1:
    return 0
  case 2:
    return 1
  default:
    return a
  }
}

func foo8(a : [Int]) {
  for v in a {
    if v > 3 {
      break
    }
    if v < 3 {
      continue
    }
  }
}

protocol P1 {
  func foo()
  func foo1()
}
class C1 : P1 {
  func foo1() {}
}
class C2 : P1 {
  func foo() {}
}
class C3 : P1 {}

protocol P2 {
	associatedtype T1
	associatedtype T2
	func foo1()
}
class C4 : P2 {}
class C5 : P2 {
  typealias T1 = Int
  func foo1() {}
}
class C6 : P2 {
  typealias T1 = Int
  typealias T2 = Int
}
class C7 : P2 {
  typealias T2 = Int
  func foo1() {}
}
class C8 : P2 {
  typealias T1 = Int
  typealias T2 = Int
  func foo1() {}
}

class C9 {}
extension C9 : P1 {}
extension C9 : P2 {}
class C10 {}
extension C10 : P1 {
  func foo() {}
  func foo1() {}
}
extension C10 : P2 {
  typealias T1 = Int
  typealias T2 = Int
  func foo1() {}
}
class C11 {}
extension C11 : P1 {
  func foo() {}
}
extension C11 : P2 {
  typealias T1 = Int
  typealias T2 = Int
}
class C12 {}
extension C12 : P1 {
  func foo1() {}
}
extension C12 : P2 {
  typealias T1 = Int
  func foo1() {}
}
class C13 {}
extension C13 : P1 {
  func foo() {}
  func foo1() {}
}
extension C13 : P2 {
  typealias T1 = Int
  func foo1() {}
}
class C14 {}
extension C14 : P1 {
  func foo() {}
}
extension C14 : P2 {
  typealias T1 = Int
  typealias T2 = Int
  func foo1() {}
}
protocol P3 {
  func foo3()
  func foo4()
}
extension C14: P3 {
  func foo3()
}

enum E {
  case e1
  case e2
  case e3
}

func foo2(_ e : E) -> Int{
  switch(e) {
  case .e1:
  case .e2:
    return 0;
  default:
    return 1;
  }
}

func testInout(_ a : inout Int) {
  var b = a + 1 + 1
  b = b + 1
  testInout(&b)
}

func testBranch1(_ a : Bool) {
  if a {
    return
  } else {
    return
  }
}

func testBranch2(_ a : Bool) {
  if a {
    return
  }
}

func testBranch3(_ a : Bool) -> Int {
  if a {
    return 0
  } else {
    return 1
  }
}

func testBranch4(_ a : Bool) -> Int {
  if a {
    return 0
  }
}

func testStringLiteral() -> String {
  let name = "Jason"
  print("Hello, \(name)!")
  return "abc"
}

func testCollapseNestedIf1() {
  let a = 3
  if a > 2 {
    if a < 10 {}
  }
}

func testCollapseNestedIf2() {
  let a = 3
  if a > 2, a != 4 {
    if a < 10 {}
  }
}

func testCollapseNestedIf3() {
  let a = 3
  if a > 2 {
    if a < 10 {}
    let b = 0
  }
}

func testCollapseNestedIf4() {
  let a = 3
  if a > 2 {
    let b = 0
    if a < 10 {}
  }
}

func testCollapseNestedIf5() {
  let a = 3
  if a > 2 {
    if a < 10 {}
  } else {
    print("else")
  }
}

func testCollapseNestedIf6() {
  let a = 3
  if a > 2 {
    if a < 10 {
      print("if")
    } else if a < 5 {
      print("else")
    }
  }
}

func testStringInterpolation() -> String {
  let name = "Jason"
  let one = "\(1)"
  let _ = "aaa" + "bbb"
  let _ = name + "Bourne"
  let _ = name + one
}

func testForceTry() {
  func throwingFunc() throws -> Int { return 3 }
  let _ = try! throwingFunc()
}

func testSwitchExpand() {
  enum AccountType {
    case savings, checking
  }
  let account: AccountType = .savings
  switch account { }
}

func testExpandTernaryExpr() {
  let a = 3
  let b = 5
  let x = a < 5 ? a : b
}

func testConvertToTernaryExpr() {
  let a = 3
  let b = 5
  let x: Int
  if a < 5 {
    x = a
  } else {
    x = b
  }
}

// RUN: %refactor -source-filename %s -pos=2:1 -end-pos=5:13 | %FileCheck %s -check-prefix=CHECK1
// RUN: %refactor -source-filename %s -pos=3:1 -end-pos=5:13 | %FileCheck %s -check-prefix=CHECK1
// RUN: %refactor -source-filename %s -pos=4:1 -end-pos=5:13 | %FileCheck %s -check-prefix=CHECK1
// RUN: %refactor -source-filename %s -pos=5:1 -end-pos=5:13 | %FileCheck %s -check-prefix=CHECK1

// RUN: %refactor -source-filename %s -pos=2:1 -end-pos=2:18 | %FileCheck %s -check-prefix=CHECK2
// RUN: %refactor -source-filename %s -pos=2:1 -end-pos=3:16 | %FileCheck %s -check-prefix=CHECK2
// RUN: %refactor -source-filename %s -pos=2:1 -end-pos=4:26 | %FileCheck %s -check-prefix=CHECK2

// RUN: %refactor -source-filename %s -pos=2:13 -end-pos=2:18 | %FileCheck %s -check-prefix=CHECK3

// RUN: %refactor -source-filename %s -pos=10:1 -end-pos=13:13 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=12:1 -end-pos=15:13 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=21:1 -end-pos=23:6 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=24:1 -end-pos=26:6 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=21:1 -end-pos=26:6 | %FileCheck %s -check-prefix=CHECK-NONE

// RUN: %refactor -source-filename %s -pos=34:8 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB
// RUN: %refactor -source-filename %s -pos=37:8 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB
// RUN: %refactor -source-filename %s -pos=40:8 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB

// RUN: %refactor -source-filename %s -pos=47:8 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB
// RUN: %refactor -source-filename %s -pos=48:8 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB
// RUN: %refactor -source-filename %s -pos=52:8 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB
// RUN: %refactor -source-filename %s -pos=56:8 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB

// RUN: %refactor -source-filename %s -pos=60:8 | %FileCheck %s -check-prefix=CHECK-RENAME-ONLY

// RUN: %refactor -source-filename %s -pos=66:8 | %FileCheck %s -check-prefix=CHECK-RENAME-ONLY
// RUN: %refactor -source-filename %s -pos=67:12 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB
// RUN: %refactor -source-filename %s -pos=68:12 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB

// RUN: %refactor -source-filename %s -pos=69:8 | %FileCheck %s -check-prefix=CHECK-RENAME-ONLY
// RUN: %refactor -source-filename %s -pos=70:12 | %FileCheck %s -check-prefix=CHECK-RENAME-ONLY
// RUN: %refactor -source-filename %s -pos=74:12 | %FileCheck %s -check-prefix=CHECK-RENAME-ONLY

// RUN: %refactor -source-filename %s -pos=79:8 | %FileCheck %s -check-prefix=CHECK-RENAME-ONLY
// RUN: %refactor -source-filename %s -pos=80:12 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB
// RUN: %refactor -source-filename %s -pos=83:12 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB

// RUN: %refactor -source-filename %s -pos=87:8 | %FileCheck %s -check-prefix=CHECK-RENAME-ONLY
// RUN: %refactor -source-filename %s -pos=88:12 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB
// RUN: %refactor -source-filename %s -pos=91:12 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB

// RUN: %refactor -source-filename %s -pos=95:8 | %FileCheck %s -check-prefix=CHECK-RENAME-ONLY
// RUN: %refactor -source-filename %s -pos=96:12 | %FileCheck %s -check-prefix=CHECK-RENAME-ONLY
// RUN: %refactor -source-filename %s -pos=100:12 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB

// RUN: %refactor -source-filename %s -pos=104:8 | %FileCheck %s -check-prefix=CHECK-RENAME-ONLY
// RUN: %refactor -source-filename %s -pos=105:12 | %FileCheck %s -check-prefix=CHECK-RENAME-ONLY
// RUN: %refactor -source-filename %s -pos=108:12 | %FileCheck %s -check-prefix=CHECK-RENAME-ONLY
// RUN: %refactor -source-filename %s -pos=117:12 | %FileCheck %s -check-prefix=CHECK-RENAME-STUB

// RUN: %refactor -source-filename %s -pos=132:6 | %FileCheck %s -check-prefix=CHECK-EXPAND-DEFAULT

// RUN: %refactor -pos=138:11 -end-pos=138:12 -source-filename %s | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -pos=138:11 -end-pos=138:20 -source-filename %s | %FileCheck %s -check-prefix=CHECK3
// RUN: %refactor -pos=139:7 -end-pos=139:8 -source-filename %s | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -pos=139:3 -end-pos=139:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -pos=140:13 -end-pos=140:15 -source-filename %s | %FileCheck %s -check-prefix=CHECK2

// RUN: %refactor -pos=144:1 -end-pos=148:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-EXTRCT-METHOD
// RUN: %refactor -pos=158:1 -end-pos=162:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-EXTRCT-METHOD
// RUN: %refactor -pos=152:1 -end-pos=154:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -pos=166:1 -end-pos=168:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-NONE

// RUN: %refactor -source-filename %s -pos=173:12 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=174:12 | %FileCheck %s -check-prefix=CHECK-LOCALIZE-STRING

// RUN: %refactor -source-filename %s -pos=173:3 -end-pos=173:27| %FileCheck %s -check-prefix=CHECK-EXTRCT-METHOD

// RUN: %refactor -source-filename %s -pos=179:3 | %FileCheck %s -check-prefix=CHECK-COLLAPSE-NESTED-IF
// RUN: %refactor -source-filename %s -pos=186:3 | %FileCheck %s -check-prefix=CHECK-COLLAPSE-NESTED-IF
// RUN: %refactor -source-filename %s -pos=193:3 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=201:3 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=209:3 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=218:3 | %FileCheck %s -check-prefix=CHECK-NONE

// RUN: %refactor -source-filename %s -pos=230:11 -end-pos=230:24 | %FileCheck %s -check-prefix=CHECK-STRINGS-INTERPOLATION
// RUN: %refactor -source-filename %s -pos=231:11 -end-pos=231:26 | %FileCheck %s -check-prefix=CHECK-STRINGS-INTERPOLATION
// RUN: %refactor -source-filename %s -pos=232:11 -end-pos=232:21 | %FileCheck %s -check-prefix=CHECK-STRINGS-INTERPOLATION

// RUN: %refactor -source-filename %s -pos=237:11 | %FileCheck %s -check-prefix=CHECK-TRY-CATCH
// RUN: %refactor -source-filename %s -pos=237:12 | %FileCheck %s -check-prefix=CHECK-TRY-CATCH
// RUN: %refactor -source-filename %s -pos=237:13 | %FileCheck %s -check-prefix=CHECK-TRY-CATCH
// RUN: %refactor -source-filename %s -pos=237:14 | %FileCheck %s -check-prefix=CHECK-TRY-CATCH

// RUN: %refactor -source-filename %s -pos=245:3 | %FileCheck %s -check-prefix=CHECK-EXPAND-SWITCH

// RUN: %refactor -source-filename %s -pos=251:3 -end-pos=251:24 | %FileCheck %s -check-prefix=CHECK-EXPAND-TERNARY-EXPRESSEXPRESSION

// RUN: %refactor -source-filename %s -pos=257:3 -end-pos=262:4 | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-TERNARY-EXPRESSEXPRESSION

// CHECK1: Action begins
// CHECK1-NEXT: Extract Method
// CHECK1-NEXT: Action ends

// CHECK2: Action begins
// CHECK2-NEXT: Action ends

// CHECK3: Action begins
// CHECK3-NEXT: Extract Expression
// CHECK3-NEXT: Extract Method
// CHECK3-NEXT: Extract Repeated Expression
// CHECK3-NEXT: Action ends

// CHECK-NONE: Action begins
// CHECK-NONE-NEXT: Action ends

// CHECK-RENAME-STUB: Action begins
// CHECK-RENAME-STUB-NEXT: Rename
// CHECK-RENAME-STUB-NEXT: Add Missing Protocol Requirements
// CHECK-RENAME-STUB-NEXT: Action ends

// CHECK-RENAME-ONLY: Action begins
// CHECK-RENAME-ONLY-NEXT: Rename
// CHECK-RENAME-ONLY-NEXT: Action ends

// CHECK-EXPAND-DEFAULT: Action begins
// CHECK-EXPAND-DEFAULT-NEXT: Expand Default
// CHECK-EXPAND-DEFAULT-NEXT: Action ends

// CHECK-EXTRCT-METHOD: Action begins
// CHECK-EXTRCT-METHOD-NEXT: Extract Method
// CHECK-EXTRCT-METHOD-NEXT: Action ends

// CHECK-LOCALIZE-STRING: Localize String

// CHECK-COLLAPSE-NESTED-IF: Collapse Nested If Statements

// CHECK-STRINGS-INTERPOLATION: Convert to String Interpolation

// CHECK-TRY-CATCH: Convert To Do/Catch

// CHECK-EXPAND-SWITCH: Expand Switch Cases

// CHECK-EXPAND-TERNARY-EXPRESSEXPRESSION: Expand Ternary Expression

// CHECK-CONVERT-TO-TERNARY-EXPRESSEXPRESSION: Convert To Ternary Expression
