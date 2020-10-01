enum E {
  case first
  case second
  case third
  case fourth
}

func checkForEnum(e: E) {
  if e == .first {
    print("first")
  } else if e == .second {
    print("second")
  }
  else {
    print("default")
  }

  let x = "some other type"
  if e == .first {
    print("first")
  } else if x == "some value" {
    print("x")
  }
}

func checkBiggerOrSmallerSign(integer: Int) {
  if integer > 0 {
    print("Positive integer")
  } else if integer < 0 {
    print("Negative integer")
  } else {
    print("Zero")
  }
}

func checkInverseCondition(e: E) {
  if .first == e {
    print("first")
  } else if .second == e {
    print("second")
  } else {
    print("default")
  }
}

func checkAvailabilityCondition() {
  if #available(iOS 13, *) {
    print("#available may only be used as condition of an 'if', 'guard' or 'while' statement")
  }
}

func checkCaseCondition() {
  let someOptional: Int? = 42
  // Match using an enumeration case pattern.
  if case .some(let x) = someOptional {
    print(x)
  }

  // Match using an optional pattern.
  if case let x? = someOptional {
    print(x)
  }
}

func checkOptionalBindingCondition(optional: String?) {
  if let unwraped = optional {
    print(unwraped)
  }

  if var unwraped = optional {
    unwraped += "!"
    print(unwraped)
  }
}

func checkConditionalList(x: Int, y: Int) -> Bool {
  if x > 0, y < 0 {
    return false
  }

  if x > 0, x < 10 {
    return true
  }
}

func checkFunctionCallExpression() {
  if checkConditionalList(x: 0, y: 0) {
    print("false")
  }
}

func checkMultipleOrConditions(e: E) {
  if e == .first || e == .second || e == .third {
    print("1-3")
  } else if e == .fourth { 
    print("4")
  } else { 
    print(">4")
  }
}

func checkMultipleAndConditions(e: E) {
  if e == .first && e == .second && e == .third {
    print("Never executed")
  } else { 
    print("Whenever")
  }
}

func checkMultipleWithUnrelatedCondition(e: E, x: Int) {
  if e == .first || x > 0 {
    print("e is the first or x is positive")
  } else { 
    print("Shouldn't convert to switch")
  }
}

func checkOperatorIsActuallyBeingUsed(e: E) {
  if e != .first {
    print("Can't use at case statement")
  }
}

func checkLabeledCondition() {
  enum Foo { case a, b, c }
  let e = Foo.a
  let f = Foo.a

  outerLabel: if e == .a {
    innerLabel: switch f {
    case .a:
      break outerLabel
    default:
      break innerLabel
    }
    print("outside switch")
  }
  print("outside if")
}

func checkEnumWithAssociatedValues() {
  enum Barcode {
    case upc(Int, Int, Int, Int)
    case qrCode(String)
  }

  let productBarcode = Barcode.upc(8, 85909, 51226, 3)
  if case .upc(let numberSystem, let manufacturer, let product, let check) = productBarcode {
    print("UPC : \(numberSystem), \(manufacturer), \(product), \(check).")
  } else if case let .qrCode(productCode) = productBarcode {
    print("QR code: \(productCode).")
  }
}

func checkEquatableProtocol(x: Int) {
  if x == 5 {
    print("5")
  } else if x == 4 {
    print("4")
  } else if 1...3 ~= x {
    print("1...3")
  } else {
    print("default")
  }

  struct MyType: Equatable { let v: String }
  let y = MyType(v: "hello")

  if y == MyType(v: "bye") {
    print("bye")
  } else if y == MyType(v: "tchau") {
    print("tchau")
  } else {
    print("default")
  }
}

// RUN: %refactor -pos=9:3 -end-pos=16:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT
// RUN: %refactor -pos=19:3 -end-pos=23:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT2
// RUN: %refactor -pos=27:3 -end-pos=33:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT2
// RUN: %refactor -pos=37:3 -end-pos=43:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT
// RUN: %refactor -pos=47:3 -end-pos=49:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT2
// RUN: %refactor -pos=55:3 -end-pos=57:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT
// RUN: %refactor -pos=60:3 -end-pos=62:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT
// RUN: %refactor -pos=66:3 -end-pos=68:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT
// RUN: %refactor -pos=70:3 -end-pos=73:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT
// RUN: %refactor -pos=77:3 -end-pos=79:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT2
// RUN: %refactor -pos=81:3 -end-pos=83:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT2
// RUN: %refactor -pos=87:3 -end-pos=89:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT2
// RUN: %refactor -pos=93:3 -end-pos=99:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT
// RUN: %refactor -pos=103:3 -end-pos=107:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT2
// RUN: %refactor -pos=111:3 -end-pos=115:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT2
// RUN: %refactor -pos=119:3 -end-pos=121:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT2
// RUN: %refactor -pos=129:3 -end-pos=137:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT
// RUN: %refactor -pos=148:3 -end-pos=152:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT
// RUN: %refactor -pos=156:3 -end-pos=164:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT
// RUN: %refactor -pos=169:3 -end-pos=175:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-CONVERT-TO-SWITCH-STATEMENT

// CHECK-CONVERT-TO-SWITCH-STATEMENT: Convert To Switch Statement

// CHECK-CONVERT-TO-SWITCH-STATEMENT2: Action begins
// CHECK-CONVERT-TO-SWITCH-STATEMENT2-NOT: Convert To Switch Statement
// CHECK-CONVERT-TO-SWITCH-STATEMENT2: Action ends

