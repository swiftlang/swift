// RUN: %target-swift-frontend -target %target-cpu-apple-macosx99.99 -typecheck %s -verify

// The compiler shouldn't emit any diagnostics even if conformance to Reflectable is visible.

public protocol Bar {}

public enum Foo: Reflectable, Bar {
	case A(Int)
	case B
}

public func consume<T: Reflectable>(_ t: T) { }
public func consumeOptional<T: Reflectable>(_ t: T?) { }

consume(Foo.A(123) as! Reflectable)
consumeOptional(Foo.A(123) as? Reflectable)
print(Foo.A(123) is Reflectable)

// Casts to a protocol composition
consume(Foo.A(123) as! Reflectable & Bar)
consumeOptional(Foo.A(123) as? Reflectable & Bar)
print(Foo.A(123) is Reflectable & Bar)
