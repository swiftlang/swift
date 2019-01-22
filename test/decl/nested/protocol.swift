// RUN: %target-typecheck-verify-swift -parse-as-library

// Protocols can be nested inside non-generic contexts,
// but cannot host nested types themselves.

// type-in-type:
protocol Proto1 {}
enum MyNamespace1 { protocol Proto1 {} }
extension Int: Proto1 {}
extension String: MyNamespace1.Proto1 {}

func testDifferent() {
  if 42 is Proto1 {} // expected-warning {{'is' test is always true}}
  if 42 is MyNamespace1.Proto1 {}

  if "hello" is Proto1 {}
  if "hello" is MyNamespace1.Proto1 {} // expected-warning {{'is' test is always true}}
}

// type-in-extension:
extension MyNamespace1 {
  protocol AnotherProto1 {}
  static func unqualifiedLookup(_: AnotherProto1) {}
}
protocol AnotherProto1 {}
extension Int: MyNamespace1.AnotherProto1 {}
extension String: AnotherProto1 {}

func testUnqualifiedLookup() {
  MyNamespace1.unqualifiedLookup(42)
  // expected-error@+1 {{argument type 'String' does not conform to expected type 'MyNamespace1.AnotherProto1'}}
  MyNamespace1.unqualifiedLookup("hello")
}

class ParentWhichDeclaresConformance: ParentWhichDeclaresConformance.Interface {
  protocol Interface: ParentWhichDeclaresConformance {}
}

// type-in-function:
func testLocalProtocols() -> (Any)->Bool {
  protocol LocalProto: Proto1 {}
  class LocalConformer: LocalProto {}
  return { $0 is LocalProto }
}

// Protocols cannot be nested inside of generic contexts.

struct OuterGeneric<D> {
  protocol InnerProtocol { // expected-error{{protocol 'InnerProtocol' cannot be nested inside a generic context}}
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ t: D) // expected-error{{use of undeclared type 'D'}}
  }
}

extension OuterGeneric {
  // expected-error@+1 {{protocol 'InvalidProtocol' cannot be nested inside a generic context}}
  protocol InvalidProtocol {}
}

struct OuterGeneric2<T> {
  enum InnerEnum {
    class InnerClass {
      // expected-error@+1 {{protocol 'InvalidProtocol' cannot be nested inside a generic context}}
      protocol InvalidProtocol {}

      func aFunction() {
        // expected-error@+1 {{type 'AnotherInvalidProtocol' cannot be nested in generic function 'aFunction()}}
        protocol AnotherInvalidProtocol {}
      }
    }
  }
}

class OuterGenericClass<T> {
  protocol InnerProtocol { // expected-error{{protocol 'InnerProtocol' cannot be nested inside a generic context}}
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ t: T) // expected-error{{use of undeclared type 'T'}}
  }
}

func testBanLocalProtocols_generic<T>(_: T) {
  // expected-error@+1 {{type 'Banned' cannot be nested in generic function 'testBanLocalProtocols_generic'}}
  protocol Banned {}
}

// However, they are allowed in non-generic classes that have generic ancestry.

class OuterNonGenericSubclass: OuterGenericClass<Int> {
  protocol InnerProtocol2 {}
}
class GenericSubclass<T>: OuterNonGenericSubclass {
  static func usesInner(_: InnerProtocol2) {}
}

// Protocols cannot contain nested types.

protocol OuterProtocol {
  associatedtype Hen
  protocol InnerProtocol { // expected-error{{type 'InnerProtocol' cannot be nested in protocol 'OuterProtocol'}}
  // expected-note@-1 {{did you mean 'InnerProtocol'?}}
    associatedtype Rooster
    func flip(_ r: Rooster)
    func flop(_ h: Hen) // expected-error{{use of undeclared type 'Hen'}}
  }
}

struct ConformsToOuterProtocol : OuterProtocol {
  typealias Hen = Int

  func f() { let _ = InnerProtocol.self }
  // expected-error@-1 {{use of unresolved identifier 'InnerProtocol'}}
}

protocol Racoon {
  associatedtype Stripes
  class Claw<T> { // expected-error{{type 'Claw' cannot be nested in protocol 'Racoon'}}
    func mangle(_ s: Stripes) {}
    // expected-error@-1 {{use of undeclared type 'Stripes'}}
  }
  struct Fang<T> { // expected-error{{type 'Fang' cannot be nested in protocol 'Racoon'}}
    func gnaw(_ s: Stripes) {}
    // expected-error@-1 {{use of undeclared type 'Stripes'}}
  }
  enum Fur { // expected-error{{type 'Fur' cannot be nested in protocol 'Racoon'}}
    case Stripes
  }
}

enum SillyRawEnum : SillyProtocol.InnerClass {}

protocol SillyProtocol {
  class InnerClass<T> {} // expected-error {{type 'InnerClass' cannot be nested in protocol 'SillyProtocol'}}
}

enum OuterEnum {
  protocol C {} // expected-note {{'C' previously declared here}}
  case C(C) // expected-error{{invalid redeclaration of 'C'}}
}

class OtherGenericClass<T> {
  protocol InnerProtocol : OtherGenericClass { }
  // expected-error@-1{{protocol 'InnerProtocol' cannot be nested inside a generic context}}
}

protocol SelfDotTest {
  func f(_: Self.Class)
  class Class {}
  // expected-error@-1{{type 'Class' cannot be nested in protocol 'SelfDotTest'}}
}

struct Outer {
  typealias E = NestedValidation.T
  protocol NestedValidation {
    typealias T = A.B
    class A { // expected-error {{type 'A' cannot be nested in protocol 'NestedValidation'}}
      typealias B = Int
    }
  }
}

struct OuterForUFI {
  @usableFromInline
  protocol Inner {
    func req()
  }
}

extension OuterForUFI.Inner {
  public func extMethod() {} // The 'public' puts this in a special path.
}

func testLookup(_ x: OuterForUFI.Inner) {
  x.req()
  x.extMethod()
}
func testLookup<T: OuterForUFI.Inner>(_ x: T) {
  x.req()
  x.extMethod()
}
