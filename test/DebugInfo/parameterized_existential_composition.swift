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

public func foo(_ a: (any Q2<Int> & R).Type) {}
public func foo(_ a: (any Q3<Int> & R).Type) {}
public func foo(_ a: (any Q2 & Q3).Type) {}
public func foo(_ a: (any Q2 & Q3<Int>).Type) {}
public func foo2(_ a: (any Q2<Int> & Q3).Type) {}
public func foo3(_ a: (any Q2<Int> & Q3<Int>).Type) {}

public struct Foo<each T, U> {
  public var a1: (repeat any P<each T, U> & R)
}
