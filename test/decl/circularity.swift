// RUN: %target-typecheck-verify-swift

// N.B. Validating the pattern binding initializer for `pickMe` used to cause
// recursive validation of the VarDecl. Check that we don't regress now that
// this isn't the case.
public struct Cyclic {
  static func pickMe(please: Bool) -> Int { return 42 }
  public static let pickMe = Cyclic.pickMe(please: true)
}

struct Node {}
struct Parameterized<Value, Format> {
  func please<NewValue>(_ transform: @escaping (_ otherValue: NewValue) -> Value) -> Parameterized<NewValue, Format> {
    fatalError()
  }
}

extension Parameterized where Value == [Node], Format == String {
  static var pickMe: Parameterized {
    fatalError()
  }
}

extension Parameterized where Value == Node, Format == String {
  static let pickMe = Parameterized<[Node], String>.pickMe.please { [$0] }
}

enum Loop: Circle {
  struct DeLoop { }
}

protocol Circle {
  typealias DeLoop = Loop.DeLoop
}

class Base {
    static func foo(_ x: Int) {}
}

class Sub: Base {
    var foo = { () -> Int in
        let x = 42
        return foo(1) // expected-error {{variable used within its own initial value}}
    }()
}
