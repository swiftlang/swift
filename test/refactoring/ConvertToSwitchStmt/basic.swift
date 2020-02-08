enum E {
  case first
  case second
  case third
  case fourth
}

func someFunc(e: E) {
  if e == .first {
    print("first")
  } else if e == .second {
    print("second")
  }
  else {
    print("default")
  }
}

func twoStringBody(e: E) {
  if e == .first {
    print("first string")
    print("second string")
  } else {
    print("default")
  }
}

func withoutElse(e: E) {
  if e == .first {
    print("first")
  } else if e == .second {
    print("second")
  } else if e == .third {
    print("third")
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

func checkCaseCondition() {
  let someOptional: Int? = 42
  if case .some(let x) = someOptional {
    print(x)
  }

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

func checkMultipleOrConditions(e: E) {
  if e == .first || e == .second || e == .third {
    print("1-3")
  } else if e == .fourth { 
    print("4")
  } else { 
    print(">4")
  }
}

enum Foo { case a, b, c }
func checkLabeledCondition(e: Foo, f: Foo) {
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

enum Barcode {
  case upc(Int, Int, Int, Int)
  case qrCode(String)
}
func checkEnumWithAssociatedValues(productBarcode: Barcode) {
  if case .upc(let numberSystem, let manufacturer, let product, let check) = productBarcode {
    print("UPC : \(numberSystem), \(manufacturer), \(product), \(check).")
  } else if case let .qrCode(productCode) = productBarcode {
    print("QR code: \(productCode).")
  }
}

struct MyType: Equatable { let v: String }
func checkEquatableProtocol(x: Int, y: MyType) {
  if x == 5 {
    print("5")
  } else if x == 4 {
    print("4")
  } else if 1...3 ~= x {
    print("1...3")
  } else {
    print("default")
  }

  if y == MyType(v: "bye") {
    print("bye")
  } else if y == MyType(v: "tchau") {
    print("tchau")
  } else {
    print("default")
  }
}

func checkEmptyBody(e: E) {
  if e == .first {}
  else if e == .second {
    print("second")
  }
}

// RUN: rm -rf %t.result && mkdir -p %t.result

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=9:3 -end-pos=16:4 > %t.result/L9-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L9-3.swift
// RUN: diff -u %S/Outputs/basic/L9-3.swift.expected %t.result/L9-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=20:3 -end-pos=25:4 > %t.result/L20-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L20-3.swift
// RUN: diff -u %S/Outputs/basic/L20-3.swift.expected %t.result/L20-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=29:3 -end-pos=35:4 > %t.result/L29-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L29-3.swift
// RUN: diff -u %S/Outputs/basic/L29-3.swift.expected %t.result/L29-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=39:3 -end-pos=45:4 > %t.result/L39-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L39-3.swift
// RUN: diff -u %S/Outputs/basic/L39-3.swift.expected %t.result/L39-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=50:3 -end-pos=52:4 > %t.result/L50-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L50-3.swift
// RUN: diff -u %S/Outputs/basic/L50-3.swift.expected %t.result/L50-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=54:3 -end-pos=56:4 > %t.result/L54-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L54-3.swift
// RUN: diff -u %S/Outputs/basic/L54-3.swift.expected %t.result/L54-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=60:3 -end-pos=62:4 > %t.result/L60-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L60-3.swift
// RUN: diff -u %S/Outputs/basic/L60-3.swift.expected %t.result/L60-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=64:3 -end-pos=67:4 > %t.result/L64-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L64-3.swift
// RUN: diff -u %S/Outputs/basic/L64-3.swift.expected %t.result/L64-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=71:3 -end-pos=77:4 > %t.result/L71-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L71-3.swift
// RUN: diff -u %S/Outputs/basic/L71-3.swift.expected %t.result/L71-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=82:3 -end-pos=90:4 > %t.result/L82-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L82-3.swift
// RUN: diff -u %S/Outputs/basic/L82-3.swift.expected %t.result/L82-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=99:3 -end-pos=103:4 > %t.result/L99-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L99-3.swift
// RUN: diff -u %S/Outputs/basic/L99-3.swift.expected %t.result/L99-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=108:3 -end-pos=116:4 > %t.result/L108-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L108-3.swift
// RUN: diff -u %S/Outputs/basic/L108-3.swift.expected %t.result/L108-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=118:3 -end-pos=124:4 > %t.result/L118-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L118-3.swift
// RUN: diff -u %S/Outputs/basic/L118-3.swift.expected %t.result/L118-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=128:3 -end-pos=131:4 > %t.result/L128-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L128-3.swift
// RUN: diff -u %S/Outputs/basic/L128-3.swift.expected %t.result/L128-3.swift
