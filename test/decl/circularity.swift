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
        // FIXME: Bogus diagnostic
        return foo(1) // expected-error {{cannot convert value of type '()' to closure result type 'Int'}}
    }()
}

extension Float {
    static let pickMe: Float = 1
}

extension SIMD3 {
  init(_ scalar: Scalar) { self.init(repeating: scalar) }
}

extension SIMD3 where SIMD3.Scalar == Float {
    static let pickMe = SIMD3(.pickMe)
}

// Test case with circular overrides
protocol P {
    associatedtype A
    // expected-note@-1 {{protocol requires nested type 'A'}}
    func run(a: A)
}

class C1 {
    func run(a: Int) {}
}

class C2: C1, P {
    // expected-note@-1 2{{through reference here}}
    override func run(a: A) {}
    // expected-error@-1 {{circular reference}}
    // expected-note@-2 {{while resolving type 'A'}}
    // expected-note@-3 2{{through reference here}}
}

// Another crash to the above
open class G1<A> {
    open func run(a: A) {}
}

class C3: G1<A>, P {
    // expected-error@-1 {{type 'C3' does not conform to protocol 'P'}}
    // expected-error@-2 {{cannot find type 'A' in scope}}
    // expected-note@-3 2{{through reference here}}
    // expected-note@-4 {{add stubs for conformance}}
    override func run(a: A) {}
    // expected-error@-1 {{circular reference}}
    // expected-note@-2 2 {{through reference here}}
    // expected-note@-3 {{while resolving type 'A'}}
}

// Another case that triggers circular override checking.
protocol P1 {
  associatedtype X = Int
  init(x: X)
}

class C4 {
  required init(x: Int) {}
}

class D4 : C4, P1 { // expected-note 4 {{through reference here}}
  required init(x: X) { // expected-error {{circular reference}}
    // expected-note@-1 {{while resolving type 'X'}}
    // expected-note@-2 2{{through reference here}}
    super.init(x: x)
  }
}

// https://github.com/apple/swift/issues/54662
// N.B. This used to compile in 5.1.
protocol P_54662 { }
class C_54662 { // expected-note {{through reference here}}
    typealias Nest = P_54662 // expected-error {{circular reference}} expected-note {{through reference here}}
}
extension C_54662: C_54662.Nest { }
