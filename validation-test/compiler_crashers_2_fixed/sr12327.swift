// RUN: %target-swift-frontend -emit-ir -O %s

protocol A {
    associatedtype Foo // Does not crash if renamed
}

protocol B {
    associatedtype Foo // Does not crash if renamed
    var aFoo: Foo { get }
}

public struct Wrapper<T> {
    let wrapped: T
}

// Removing this extension or combining it with the next one prevents the crash
extension Wrapper: A where T: A {
    typealias Foo = Wrapper<T.Foo>
}

extension Wrapper: B where T: B {
    var aFoo: Wrapper<T.Foo> {
        return .init(wrapped: wrapped.aFoo)
    }
}

public struct Model: B {
    public struct Foo {}

    public var aFoo: Foo {
        return Foo()
    }
}

// Attempting to specialize this method for Wrapper<Model> crashes the compiler
func fooString<Body: B>(body: Body) -> String {
    return "\(body.aFoo)"
}

public func foo(_ command: Wrapper<Model>) -> String {
    return fooString(body: command)
}
