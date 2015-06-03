// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

enum Bewl : BooleanType {
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
  print("truthy ", appendNewline: false)
  return .True
}

func falsy() -> Bewl {
  print("falsy ", appendNewline: false)
  return .False
}

func logicValueTests() {
  // Logic values should convert to bool.
  struct X : BooleanType {
    var boolValue: Bool { return false }
  }
  var anX = X()
  print("BooleanType Bool = \(Bool(anX))")   // CHECK: BooleanType Bool = false

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
