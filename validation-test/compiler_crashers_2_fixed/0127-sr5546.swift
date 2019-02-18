// RUN: not %target-swift-frontend %s -typecheck
// REQUIRES: asserts

public struct Foo<A, B, C> {}

public protocol P {
    associatedtype PA
    associatedtype PB = Never
    associatedtype PC = Never

    typealias Context = Foo<PA, PB, PC>

    func f1(_ x: Context, _ y: PA)
    func f2(_ x: Context, _ y: PB)
    func f3(_ x: Context, _ y: PC)
    func f4(_ x: Context)
}

public extension P {
    public func f1(_ x: Context, _ y: PA) {
    }
    public func f2(_ x: Context, _ y: PB) {
    }
    public func f3(_ x: Context, _ y: PC) {
    }
    public func f4(_ x: Context) {
    }
}

public struct S: P {
    public typealias PA = String
    public typealias PB = Int

    public func f1(_ x: Context, _ y: PA) {
    }
    public func f2(_ x: Context, _ y: PB) {
    }
}
