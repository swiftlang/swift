// RUN: %target-swift-frontend -typecheck %s -verify

public enum Foo: Reflectable {
	case A(Int)
	case B
}

public func consume<T: Reflectable>(_ t: T) { }
public func consumeOptional<T: Reflectable>(_ t: T?) { }

consume(Foo.A(123) as! Reflectable)
consumeOptional(Foo.A(123) as? Reflectable)
print(Foo.A(123) is Reflectable)