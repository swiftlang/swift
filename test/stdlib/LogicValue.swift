// RUN: %swift -i %s | FileCheck %s
// REQUIRES: swift_interpreter

enum Bewl : LogicValue {
  case False, True

  func getLogicValue() -> Bool {
    switch self {
    case .False:
      return false
    case .True:
      return true
    }
  }
}

func truthy() -> Bewl {
  print("truthy ")
  return .True
}

func falsy() -> Bewl {
  print("falsy ")
  return .False
}

func logicValueTests() {
  // Logic values should convert to bool.
  struct X : LogicValue {
    func getLogicValue() -> Bool { return false }
  }
  var anX = X()
  println("LogicValue Bool = \(Bool(anX))")   // CHECK: LogicValue Bool = false

  println("\(!Bewl.True)") // CHECK: false
  println("\(!Bewl.False)") // CHECK: true

  // Test short-circuiting operators
  println("\(Bool(truthy() && truthy()))") // CHECK: truthy truthy true
  println("\(Bool(truthy() && falsy()))") // CHECK: truthy falsy false
  println("\(Bool(falsy() && truthy()))") // CHECK: falsy false
  println("\(Bool(falsy() && falsy()))") // CHECK: falsy false

  println("\(Bool(truthy() || truthy()))") // CHECK: truthy true
  println("\(Bool(truthy() || falsy()))") // CHECK: truthy true
  println("\(Bool(falsy() || truthy()))") // CHECK: falsy truthy true
  println("\(Bool(falsy() || falsy()))") // CHECK: falsy falsy false
}

logicValueTests()
