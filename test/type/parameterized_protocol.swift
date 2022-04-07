// RUN: %target-typecheck-verify-swift -requirement-machine-protocol-signatures=on -requirement-machine-inferred-signatures=on -disable-availability-checking

// RUN: not %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on -requirement-machine-inferred-signatures=on -disable-availability-checking 2>&1 | %FileCheck %s


/// Test some invalid syntax first

protocol Invalid1<> {}
// expected-error@-1 {{expected an identifier to name primary associated type}}

protocol Invalid2<A B> {}
// expected-error@-1 {{expected '>' to complete primary associated type list}}
// expected-note@-2 {{to match this opening '<'}}
// expected-error@-3 {{an associated type named 'A' must be declared in the protocol 'Invalid2' or a protocol it inherits}}

protocol Invalid3<Element, +> {}
// expected-error@-1 {{expected an identifier to name primary associated type}}
// expected-error@-2 {{expected '>' to complete primary associated type list}}
// expected-note@-3 {{to match this opening '<'}}
// expected-error@-4 {{an associated type named 'Element' must be declared in the protocol 'Invalid3' or a protocol it inherits}}

protocol Invalid4<Element> {}
// expected-error@-1 {{an associated type named 'Element' must be declared in the protocol 'Invalid4' or a protocol it inherits}}

protocol Invalid5<Element, Element> {
// expected-error@-1 {{duplicate primary associated type name 'Element'}}
  associatedtype Element
}

/// Test semantics

protocol Sequence<Element> {
  associatedtype Element
  // expected-note@-1 {{protocol requires nested type 'Element'; do you want to add it?}}
}

extension Sequence {
  func map<Other>(_ transform: (Self.Element) -> Other) -> ConcreteSequence<Other> {
    return ConcreteSequence<Other>()
  }
}

struct ConcreteSequence<Element> : Sequence {}

protocol EquatableSequence<Element> {
  associatedtype Element : Equatable
}

struct ConcreteEquatableSequence<Element : Equatable> : EquatableSequence {}


/// Parametrized protocol in protocol inheritance clause

// CHECK-LABEL: parameterized_protocol.(file).IntSequence@
// CHECK: Requirement signature: <Self where Self : Sequence, Self.[Sequence]Element == Int>
protocol IntSequence : Sequence<Int> {}


/// Concrete types cannot inherit from a parameterized protocol

struct SillyStruct : Sequence<Int> {}
// expected-error@-1 {{cannot inherit from protocol type with generic argument 'Sequence<Int>'}}
// expected-error@-2 {{type 'SillyStruct' does not conform to protocol 'Sequence'}}


/// Parametrized protocol in generic parameter inheritance clause

// CHECK-LABEL: parameterized_protocol.(file).IntSequenceWrapper@
// CHECK: Generic signature: <S where S : Sequence, S.[Sequence]Element == Int>
struct IntSequenceWrapper<S : Sequence<Int>> {}

// CHECK-LABEL: parameterized_protocol.(file).SequenceWrapper@
// CHECK: Generic signature: <S, E where S : Sequence, E == S.[Sequence]Element>
struct SequenceWrapper<S : Sequence<E>, E> {}


/// Parametrized protocol in associated type inheritance clause

// CHECK-LABEL: parameterized_protocol.(file).IntSequenceWrapperProtocol@
// CHECK: Requirement signature: <Self where Self.[IntSequenceWrapperProtocol]S : Sequence, Self.[IntSequenceWrapperProtocol]S.[Sequence]Element == Int>
protocol IntSequenceWrapperProtocol {
  associatedtype S : Sequence<Int>
}

// CHECK-LABEL: parameterized_protocol.(file).SequenceWrapperProtocol@
// CHECK: Requirement signature: <Self where Self.[SequenceWrapperProtocol]E == Self.[SequenceWrapperProtocol]S.[Sequence]Element, Self.[SequenceWrapperProtocol]S : Sequence>
protocol SequenceWrapperProtocol {
  associatedtype S : Sequence<E>
  associatedtype E
}

// https://bugs.swift.org/browse/SR-15979 - the GenericSignatureBuilder doesn't like this protocol.

// CHECK-LABEL: .Recursive@
// CHECK-NEXT: Requirement signature: <Self where Self.[Recursive]B == Self.[Recursive]D.[Recursive]B, Self.[Recursive]C == Self.[Recursive]D.[Recursive]C, Self.[Recursive]D : Recursive>
protocol Recursive<B, C> {
  associatedtype B
  associatedtype C
  associatedtype D: Recursive<B, C> = Self
}

/// Parametrized protocol in where clause of concrete type

// CHECK-LABEL: parameterized_protocol.(file).IntSequenceWrapper2@
// CHECK: Generic signature: <S where S : Sequence, S.[Sequence]Element == Int>
struct IntSequenceWrapper2<S> where S : Sequence<Int> {}

// CHECK-LABEL: parameterized_protocol.(file).SequenceWrapper2@
// CHECK: Generic signature: <S, E where S : Sequence, E == S.[Sequence]Element>
struct SequenceWrapper2<S, E> where S : Sequence<E> {}


/// Parametrized protocol in where clause of associated type

// CHECK-LABEL: parameterized_protocol.(file).IntSequenceWrapperProtocol2@
// CHECK: Requirement signature: <Self where Self.[IntSequenceWrapperProtocol2]S : Sequence, Self.[IntSequenceWrapperProtocol2]S.[Sequence]Element == Int>
protocol IntSequenceWrapperProtocol2 {
  associatedtype S where S : Sequence<Int>
}

// CHECK-LABEL: parameterized_protocol.(file).SequenceWrapperProtocol2@
// CHECK: Requirement signature: <Self where Self.[SequenceWrapperProtocol2]E == Self.[SequenceWrapperProtocol2]S.[Sequence]Element, Self.[SequenceWrapperProtocol2]S : Sequence>
protocol SequenceWrapperProtocol2 {
  associatedtype S where S : Sequence<E>
  associatedtype E
}


/// Multiple primary associated types
protocol Collection<Element, Index> : Sequence {
  associatedtype Index
}

func testCollectionBad1<T : Collection<String>>(_: T) {}
// expected-error@-1 {{protocol type 'Collection' specialized with too few type arguments (got 1, but expected 2)}}

func testCollectionBad2<T : Collection<String, Int, Float>>(_: T) {}
// expected-error@-1 {{protocol type 'Collection' specialized with too many type arguments (got 3, but expected 2)}}

// CHECK-LABEL: .testCollectionGood@
// CHECK-NEXT: Generic signature: <T where T : Collection, T.[Sequence]Element == String, T.[Collection]Index == Int>
func testCollectionGood<T : Collection<String, Int>>(_: T) {}


/// Parametrized protocol in opaque result type

struct OpaqueTypes<E> {
  func returnSequenceOfInt() -> some Sequence<Int> {
    return ConcreteSequence<Int>()
  }

  func returnSequenceOfE() -> some Sequence<E> {
    return ConcreteSequence<E>()
  }

  // Invalid
  func returnSequenceOfIntBad() -> some Sequence<Int> {
    // expected-note@-1 {{opaque return type declared here}}
    return ConcreteSequence<E>()
    // expected-error@-1 {{return type of instance method 'returnSequenceOfIntBad()' requires that 'E' conform to 'Int'}}
  }

  // Invalid
  func returnEquatableSequenceBad() -> some Sequence<E> {
    return ConcreteEquatableSequence<E>()
    // expected-error@-1 {{type 'E' does not conform to protocol 'Equatable'}}
  }
}


/// Extensions of parameterized protocol type

// CHECK-LABEL: ExtensionDecl line={{[0-9]+}} base=Sequence<Int>
// CHECK: Generic signature: <Self where Self : Sequence, Self.[Sequence]Element == Int>
extension Sequence<Int> {

  // CHECK-LABEL: parameterized_protocol.(file).Sequence extension.doSomethingGeneric@
  // CHECK: Generic signature: <Self, E where Self : Sequence, Self.[Sequence]Element == Int>
  func doSomethingGeneric<E>(_: E) {}
}


/// Cannot use parameterized protocol as the type of a value

func takesSequenceOfInt1(_: Sequence<Int>) {}
// expected-error@-1 {{protocol type with type arguments can only be used as a generic constraint}}

func returnsSequenceOfInt1() -> Sequence<Int> {}
// expected-error@-1 {{protocol type with type arguments can only be used as a generic constraint}}

func takesSequenceOfInt2(_: any Sequence<Int>) {}
// expected-error@-1 {{protocol type with type arguments can only be used as a generic constraint}}

func returnsSequenceOfInt2() -> any Sequence<Int> {}
// expected-error@-1 {{protocol type with type arguments can only be used as a generic constraint}}

func typeExpr() {
  _ = Sequence<Int>.self
  // expected-error@-1 {{protocol type with type arguments can only be used as a generic constraint}}

  _ = any Sequence<Int>.self
  // expected-error@-1 {{protocol type with type arguments can only be used as a generic constraint}}
}

/// Not supported as a protocol composition term for now

protocol SomeProto {}

func protocolCompositionNotSupported(_: SomeProto & Sequence<Int>) {}
// expected-error@-1 {{non-protocol, non-class type 'Sequence<Int>' cannot be used within a protocol-constrained type}}
