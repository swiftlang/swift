// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

enum Bewl : Boolean {
  case False, True

  var boolValue: Bool {
    switch self {
    case .False:
      return false
    case .True:
      return true
    }
  }
}

func truthy() -> Bewl {
  print("truthy ", terminator: "")
  return .True
}

func falsy() -> Bewl {
  print("falsy ", terminator: "")
  return .False
}

func logicValueTests() {
  // Logic values should convert to bool.
  struct X : Boolean {
    var boolValue: Bool { return false }
  }
  var anX = X()
  print("Boolean Bool = \(Bool(anX))")   // CHECK: Boolean Bool = false

  print("\(!Bewl.True)") // CHECK: false
  print("\(!Bewl.False)") // CHECK: true

  // Test short-circuiting operators
  print("\(Bool(truthy() && truthy()))") // CHECK: truthy truthy true
  print("\(Bool(truthy() && falsy()))") // CHECK: truthy falsy false
  print("\(Bool(falsy() && truthy()))") // CHECK: falsy false
  print("\(Bool(falsy() && falsy()))") // CHECK: falsy false

  print("\(Bool(truthy() || truthy()))") // CHECK: truthy true
  print("\(Bool(truthy() || falsy()))") // CHECK: truthy true
  print("\(Bool(falsy() || truthy()))") // CHECK: falsy truthy true
  print("\(Bool(falsy() || falsy()))") // CHECK: falsy falsy false
}

logicValueTests()
