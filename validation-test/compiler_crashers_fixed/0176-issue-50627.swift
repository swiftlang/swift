// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/50627

@objc protocol Foo {
    associatedtype Bar
    var property: Generic<Bar> { get }
}

class Generic<Element> {
}

class FooImpl<T>: NSObject, Foo {
    let property: Generic<T>
}

