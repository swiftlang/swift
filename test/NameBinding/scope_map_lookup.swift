// RUN: %target-typecheck-verify-swift -enable-astscope-lookup

// Name binding in default arguments

// FIXME: Semantic analysis should produce an error here, because 'x'
// is not actually available.
func functionParamScopes(x: Int, y: Int = x) -> Int {
  return x + y
}

// Name binding in instance methods.
class C1 {
	var x = 0

  var hashValue: Int {
    return x
  }
}

// Protocols involving 'Self'.
protocol P1 {
  associatedtype A = Self
}

// Protocols involving associated types.
protocol AProtocol {
  associatedtype e : e
  // expected-error@-1 {{inheritance from non-protocol, non-class type 'Self.e'}}
  // expected-error@-2 {{type 'Self.e' constrained to non-protocol, non-class type 'Self.e'}}
}

// Extensions.
protocol P2 {
}

extension P2 {
  func getSelf() -> Self {
    return self
  }
}

#if false
// Lazy properties
class LazyProperties {
  init() {
    lazy var localvar = 42  // FIXME: should error {{lazy is only valid for members of a struct or class}} {{5-10=}}
    localvar += 1
    _ = localvar
  }

  var value: Int = 17

  lazy var prop1: Int = value

  lazy var prop2: Int = { value + 1 }()

  lazy var prop3: Int = { [weak self] in self.value + 1 }()

  lazy var prop4: Int = self.value

  lazy var prop5: Int = { self.value + 1 }()
}
#endif

// Protocol extensions.
// Extending via a superclass constraint.
class Superclass {
  func foo() { }
  static func bar() { }

  typealias Foo = Int
}

protocol PConstrained4 { }

extension PConstrained4 where Self : Superclass {
  func testFoo() -> Foo {
    foo()
    self.foo()

    return Foo(5)
  }

  static func testBar() {
    bar()
    self.bar()
  }
}

// Local computed properties.
func localComputedProperties() {
  var localProperty: Int {
    get {
      return localProperty // expected-warning{{attempting to access 'localProperty' within its own getter}}
    }
    set {
      _ = newValue
      print(localProperty)
    }
  }
  { print(localProperty) }()
}

// Top-level code.
func topLevel() { }

topLevel()

let c1opt: C1? = C1()
guard let c1 = c1opt else { }

protocol Fooable {
  associatedtype Foo

  var foo: Foo { get }
}

// The extension below once caused infinite recursion.
struct S<T> // expected-error{{expected '{' in struct}}
extension S // expected-error{{expected '{' in extension}}

let a = b ; let b = a
// expected-note@-1 {{'a' declared here}}
// expected-error@-2 {{ambiguous use of 'a'}}
