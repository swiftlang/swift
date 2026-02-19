// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -typecheck %s -serialize-diagnostics-path %t/output.sarif 2>&1
// RUN: %S/validate-sarif.py %S/Outputs/locations.sarif %t/output.sarif %s

class MyClass {
  let x: Int = "wrong"  // Error in class property

  func myMethod() {
    let y: Int = "wrong"  // Error in class method
  }

  init() {
    let z: Int = "wrong"  // Error in class initializer
  }

  deinit {
    let w: Int = "wrong"  // Error in class deinitializer
  }

  subscript(index: Int) -> String {
    let s: Int = "wrong"  // Error in class subscript
    return "test"
  }

  var computed: Int {
    get {
      let c: Int = "wrong"  // Error in class property getter
      return 0
    }
    set {
      let d: Int = "wrong"  // Error in class property setter
    }
  }
}

// MARK: - Struct contexts

struct MyStruct {
  let x: Int = "wrong"  // Error in struct property

  func myMethod() {
    let y: Int = "wrong"  // Error in struct method
  }

  init() {
    let z: Int = "wrong"  // Error in struct initializer
  }

  subscript(index: Int) -> String {
    let s: Int = "wrong"  // Error in struct subscript
    return "test"
  }
}

// MARK: - Enum contexts

enum MyEnum {
  case first
  case second(Int)

  func myMethod() {
    let y: Int = "wrong"  // Error in enum method
  }

  var computed: Int {
    let c: Int = "wrong"  // Error in enum property
    return 0
  }
}

// MARK: - Actor contexts

actor MyActor {
  let x: Int = "wrong"  // Error in actor property

  func myMethod() {
    let y: Int = "wrong"  // Error in actor method
  }

  init() {
    let z: Int = "wrong"  // Error in actor initializer
  }
}

// MARK: - Protocol contexts

protocol MyProtocol {
  associatedtype Item

  func myMethod()
  var myProperty: Int { get }
}

// MARK: - Extension contexts

extension MyClass {
  func extMethod() {
    let x: Int = "wrong"  // Error in extension method
  }
}

// MARK: - Nested type contexts

class OuterClass {
  class NestedClass {
    func nestedMethod() {
      let x: Int = "wrong"  // Error in nested class method
    }
  }

  struct NestedStruct {
    func nestedMethod() {
      let y: Int = "wrong"  // Error in nested struct method
    }
  }

  enum NestedEnum {
    case value

    func nestedMethod() {
      let z: Int = "wrong"  // Error in nested enum method
    }
  }
}

// MARK: - Type alias contexts

typealias MyAlias = String

// MARK: - Operator contexts

infix operator +++

func +++ (lhs: Int, rhs: Int) -> Int {
  let x: Int = "wrong"  // Error in operator function
  return lhs + rhs
}

// MARK: - Precedence group contexts

precedencegroup MyPrecedence {
  higherThan: AdditionPrecedence
}

// MARK: - Macro contexts (if supported)

@freestanding(expression)
macro myMacro<T>(_ value: T) -> T = #externalMacro(module: "MyMacros", type: "MyMacro")

// MARK: - Free functions (top-level)

func topLevelFunction() {
  let x: Int = "wrong"  // Error in top-level function
}

// MARK: - Nested functions

func outerFunction() {
  func innerFunction() {
    let x: Int = "wrong"  // Error in nested function
  }
}

// MARK: - Closures within functions

func functionWithClosure() {
  let closure = {
    let x: Int = "wrong"  // Error in closure (may not generate logical location)
  }
}

// MARK: - Property with accessor

struct StructWithAccessors {
  var value: Int {
    willSet {
      let x: Int = "wrong"  // Error in willSet accessor
    }
    didSet {
      let y: Int = "wrong"  // Error in didSet accessor
    }
  }
}
