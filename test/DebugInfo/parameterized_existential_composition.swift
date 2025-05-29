// RUN: %target-swift-frontend -emit-ir %s -g -target %target-swift-5.9-abi-triple

// Note: The goal of this test is to exercise the mangling/demangling via
// the -g flag.

public protocol P<A, B> {
  associatedtype A
  associatedtype B
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

public func foo(_ a: (any P & R).Type) {}
public func foo(_ a: (any P & Q<String>).Type) {}
public func foo(_ a: (any P & Q<String> & R).Type) {}
public func foo(_ a: (any P & Q<String> & R & C<Bool>).Type) {}
public func foo(_ a: (any P & Q<String> & R & AnyObject).Type) {}

public func foo(_ a: (any P<Int, Float> & Q).Type) {}
public func foo(_ a: (any P<Int, Float> & Q & R).Type) {}
public func foo(_ a: (any P<Int, Float> & Q & R & C<Bool>).Type) {}
public func foo(_ a: (any P<Int, Float> & Q & R & AnyObject).Type) {}

public struct Foo<each T, U> {
  public var a1: (repeat any P<each T, U> & R)
}
