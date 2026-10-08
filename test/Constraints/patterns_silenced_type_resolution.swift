// RUN: %target-typecheck-verify-swift -swift-version 5

// To find out whether a pattern such as 'A.b' names an enum case, pattern
// resolution first resolves 'A' as a type with diagnostics silenced. If that
// fails, the pattern is type-checked as an expression instead. The silenced
// type resolution must not diagnose anything, so that each problem below is
// reported once, by the expression.

protocol PA {
  associatedtype Assoc
}

// A dependent member type that does not exist.
func dependentMember<T: PA>(_ x: Any, _: T) {
  switch x {
  case T.Missing.a: // expected-error {{type 'T' has no member 'Missing'}}
    break
  default:
    break
  }
}

// A nested type whose contextual requirements are not satisfied.
struct Outer<T> {}

extension Outer where T == Int { // expected-note {{requirement specified as 'T' == 'Int' [with T = T]}}
  enum Nested {
    case a
  }
}

extension Outer {
  func contextualRequirements(_ x: Any) {
    switch x {
    case Nested.a: // expected-error {{'Outer<T>.Nested' requires the types 'T' and 'Int' be equivalent}}
      break
    default:
      break
    }
  }
}

// A type pack referenced without 'each'.
func packReference<each T>(_ x: Any, _: repeat each T) {
  switch x {
  case T.a: // expected-error {{type pack 'T' must be referenced with 'each'}}
    break
  default:
    break
  }
}

// An associated type of an existential base. The unsilenced resolution reports
// it and carries on, so the silenced one must fail instead of succeeding.
enum Box<T> {
  case a
}

protocol HasB {
  associatedtype B
}

func existentialMember<T>(_ x: Any, _: T) where T == any HasB {
  // expected-warning@-1 {{same-type requirement makes generic parameter 'T' non-generic}}
  switch x {
  case Box<T.B>.a: // expected-error {{cannot access associated type 'B' from 'any HasB'; use a concrete type or generic parameter base instead}}
    break
  default:
    break
  }
}

// The generic arguments below are shared by the silenced resolution and the
// expression. The silenced resolution must not mark them as diagnosed, or the
// expression reports nothing.
protocol P1 {}
class C1 {}
class C2 {}
typealias ExistentialHasB = any HasB

func sharedGenericArguments(_ x: Any) {
  switch x {
  case Box<Int & P1>.a: // expected-error {{non-protocol, non-class type 'Int' cannot be used within a protocol-constrained type}}
    break
  case Box<C1 & C2>.a: // expected-error {{protocol-constrained type cannot contain class 'C2' because it already contains class 'C1'}}
    break
  case Box<(~P1)>.a: // expected-error {{type 'P1' cannot be suppressed}}
    break
  case Box<ExistentialHasB.B>.a: // expected-error {{cannot access associated type 'B' from 'ExistentialHasB' (aka 'any HasB'); use a concrete type or generic parameter base instead}}
    break
  case Box<Swift>.a: // expected-error {{cannot find type 'Swift' in scope}} expected-note {{cannot use module 'Swift' as a type}}
    break
  default:
    break
  }
}

protocol HasT1 { typealias T = Int } // expected-note {{found this candidate}}
protocol HasT2 { typealias T = String } // expected-note {{found this candidate}}

struct AmbiguousGenericArgument: HasT1, HasT2 {
  func f(_ x: Any) {
    switch x {
    case Box<T>.a: // expected-error {{'T' is ambiguous for type lookup in this context}}
      break
    default:
      break
    }
  }
}

// Generic arguments whose problems are found while applying them.
protocol NoPrimaryAssociatedTypes {} // expected-note {{'NoPrimaryAssociatedTypes' declared here}}
struct NumericOnly<T: Numeric> {
  struct Inner {}
}

func genericArgumentProblems(_ x: Any) {
  switch x {
  case Box<NoPrimaryAssociatedTypes<Int>>.a: // expected-error {{protocol 'NoPrimaryAssociatedTypes' does not have primary associated types that can be constrained}}
    break
  case Box<NumericOnly<String>.Inner>.a: // expected-error {{type 'String' does not conform to protocol 'Numeric'}}
    break
  case Box<Deinitable>.a: // expected-error {{'Deinitable' is reserved for use by the compiler}}
    break
  default:
    break
  }
}

// Inverses that conflict with the rest of a composition.
enum NoncopyableBox<T: ~Copyable> {
  case a
}

func conflictingInverses(_ x: Any) {
  switch x {
  case NoncopyableBox<Copyable & ~Copyable>.a: // expected-error {{composition cannot contain '~Copyable' when another member requires 'Copyable'}}
    break
  case NoncopyableBox<AnyObject & ~Copyable>.a: // expected-error {{composition involving 'AnyObject' cannot contain '~Copyable'}}
    break
  default:
    break
  }
}

// Problems that the unsilenced resolution reports while still forming a type.
// The silenced resolution must fail instead, or the pattern is accepted.
struct NotHashable {}

func reportedButFormed(_ x: Any) {
  switch x {
  case Box<Int!>.a: // expected-error {{using '!' is not allowed here}} expected-note {{use '?' instead}}
    break
  case Box<() throws(Int) -> Void>.a:
    // expected-error@-1 {{thrown type 'Int' does not conform to the 'Error' protocol}}
    // expected-error@-2 {{type 'Any' cannot conform to 'Equatable'}}
    // expected-note@-3 {{only concrete types such as structs, enums and classes can conform to protocols}}
    // expected-note@-4 {{required by operator function '~=' where 'T' = 'Any'}}
    break
  case Box<[NotHashable: Int]>.a: // expected-error {{type 'NotHashable' does not conform to protocol 'Hashable'}}
    break
  default:
    break
  }
}

// A thrown type whose conformance to 'Error' cannot be checked yet, because
// pattern resolution lets the unbound generic type through.
struct GenericError<T> {}
extension GenericError: Error where T: Hashable {}

func unboundThrownType(_ x: Box<Int>) {
  switch x {
  case Box<() throws(GenericError) -> Void>.a:
    // expected-error@-1 {{generic enum type 'Box<() throws(GenericError) -> Void>' is ambiguous without explicit generic parameters when matching value of type 'Box<Int>'}}
    break
  default:
    break
  }
}
