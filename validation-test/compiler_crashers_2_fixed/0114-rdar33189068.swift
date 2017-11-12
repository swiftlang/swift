// RUN: %target-swift-frontend %s -typecheck

struct Bar : BarProtocol {
    typealias Element = Int
}

struct Foo: FooProtocol {
    typealias Things = Bar
    func thing() -> Thing {}
}

protocol BarProtocol {
    associatedtype Element
}

protocol FooProtocol {
    associatedtype Things: BarProtocol
    typealias Thing = Things.Element
}
