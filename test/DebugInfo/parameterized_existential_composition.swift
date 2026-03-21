// RUN: %target-swift-frontend -emit-ir %s -g -target %target-swift-5.9-abi-triple

// Note: The goal of this test is to exercise the mangling/demangling via
// the -g flag.

public protocol P<A, B> {
  associatedtype A
  associatedtype B
}

public protocol P1<A> {
  associatedtype A
}

public protocol Q<C> {
  associatedtype C
}

public protocol R {}

public class C<T: Equatable> {}

public func foo(_ a: any P<Int, Float> & R) {}
public func foo(_ a: any P<Int, Float> & Q<String>) {}
public func foo(_ a: any P<Int, Float> & Q<String> & R) {}
public func foo(_ a: any P<Int, Float> & Q<String> & R & C<Bool>) {}
public func foo(_ a: any P<Int, Float> & Q<String> & R & AnyObject) {}

public func foo(_ a: any (P<Int, Float> & R).Type) {}
public func foo(_ a: any (P<Int, Float> & Q<String>).Type) {}
public func foo(_ a: any (P<Int, Float> & Q<String> & R).Type) {}
public func foo(_ a: any (P<Int, Float> & Q<String> & R & C<Bool>).Type) {}
public func foo(_ a: any (P<Int, Float> & Q<String> & R & AnyObject).Type) {}

public func foo(_ a: (any P<Int, Float> & R).Type) {}
public func foo(_ a: (any P<Int, Float> & Q<String>).Type) {}
public func foo(_ a: (any P<Int, Float> & Q<String> & R).Type) {}
public func foo(_ a: (any P<Int, Float> & Q<String> & R & C<Bool>).Type) {}
public func foo(_ a: (any P<Int, Float> & Q<String> & R & AnyObject).Type) {}

public func foo(_ a: (any P & P1).Type) {}
public func foo(_ a: (any P & P1<String>).Type) {}
public func foo(_ a: (any P & P1<String> & R).Type) {}
public func foo(_ a: (any P & P1<String> & R & C<Bool>).Type) {}
public func foo(_ a: (any P & P1<String> & R & AnyObject).Type) {}

public func foo(_ a: (any P<Int, Float> & P1).Type) {}
public func foo(_ a: (any P<Int, Float> & P1 & R).Type) {}
public func foo(_ a: (any P<Int, Float> & P1 & R & C<Bool>).Type) {}
public func foo(_ a: (any P<Int, Float> & P1 & R & AnyObject).Type) {}

public protocol Q2<C>: Q {}

public protocol Q3<C>: Q {
  associatedtype C
}

// Exercise a constrained existential composition where one protocol inherits
// the parameterized base protocol.
public protocol Base<T> {
  associatedtype T
}

public protocol Derived: Base {}

public func foo(_ a: (any Q2<Int> & R).Type) {}
public func foo(_ a: (any Q3<Int> & R).Type) {}
public func foo(_ a: (any Q2 & Q3).Type) {}
public func foo(_ a: (any Q2 & Q3<Int>).Type) {}
public func foo2(_ a: (any Q2<Int> & Q3).Type) {}
public func foo3(_ a: (any Q2<Int> & Q3<Int>).Type) {}

public func foo4<T>(_ a: any Derived & Base<T>) {}
public func foo5<T>(_ a: (any Derived & Base<T>).Type) {}
public func foo6<T>() -> any Derived & Base<T> { fatalError() }

// Exercise typealias-based constrained existential compositions where a
// derived protocol is composed with one of its inherited parameterized
// base protocols.
public typealias FullProtocolWithU<U> = FullProtocol & ProtocolGenericU<U>
public typealias FullProtocolWithT<T> = FullProtocol & ProtocolGenericT<T>

public protocol FullProtocol<U, T>: ProtocolGenericU, ProtocolGenericT {}
public protocol ProtocolGenericU<U>: BaseProtocol {}
public protocol ProtocolGenericT<T>: BaseProtocol {}

public protocol BaseProtocol {
  associatedtype U
  associatedtype T
}

public func crashU<U>(_ value: any FullProtocolWithU<U>) {}
public func crashT<T>(_ value: any FullProtocolWithT<T>) {}

public func crashMetaU<U>(_ value: (any FullProtocolWithU<U>).Type) {}
public func crashMetaT<T>(_ value: (any FullProtocolWithT<T>).Type) {}

public func returnU<U>() -> any FullProtocolWithU<U> { fatalError() }
public func returnT<T>() -> any FullProtocolWithT<T> { fatalError() }

// Exercise a constrained existential composition that must recover multiple
// inherited parameterized protocols in the same order they originally
// appeared in the typealias composition.
public typealias HopToSuccessorWithDestination<Successor, Destination> =
  Hop & HopGenericSuccessor<Successor> & HopGenericDestination<Destination>

public protocol Hop<Parent, Successor>:
  HopGenericParent, HopGenericSuccessor, HopGenericDestination {}

public protocol HopGenericDestination<Destination>: HopBase {}
public protocol HopGenericSuccessor<Successor>: HopBase {}
public protocol HopGenericParent<Parent>: HopBase {}

public protocol HopBase {
  associatedtype Parent
  associatedtype Successor
  associatedtype Destination
}

public func crashHop<Successor, Destination>(
  _ value: (any HopToSuccessorWithDestination<Successor, Destination>)?
) {}

public func crashHopMeta<Successor, Destination>(
  _ value: ((any HopToSuccessorWithDestination<Successor, Destination>)?).Type
) {}

public func returnHop<Successor, Destination>()
  -> (any HopToSuccessorWithDestination<Successor, Destination>)? {
  fatalError()
}

// Exercise a constrained existential composition where the most-specific
// inherited protocol requires more primary associated type constraints than are
// available, so reconstruction must recover the viable root-only inherited
// protocol instead.
public typealias WritableWithRoot<Root> = Writable & GenericRoot<Root>

public protocol Writable<Root, Value>: Full {}
public protocol Full<Root, Value>: GenericRoot, GenericValue {}
public protocol GenericRoot<Root>: TwoFieldBase {}
public protocol GenericValue<Value>: TwoFieldBase {}

public protocol TwoFieldBase {
  associatedtype Root
  associatedtype Value
}

public func crashWritableRoot<Root>(_ value: any WritableWithRoot<Root>) {}
public func crashWritableRootMeta<Root>(_ value: (any WritableWithRoot<Root>).Type) {}
public func returnWritableRoot<Root>() -> any WritableWithRoot<Root> { fatalError() }

public struct Foo<each T, U> {
  public var a1: (repeat any P<each T, U> & R)
}
