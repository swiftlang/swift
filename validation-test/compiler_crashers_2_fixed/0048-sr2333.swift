// RUN: %target-swift-frontend %s -emit-ir

public protocol Proto {
    associatedtype One
    associatedtype Two

    static func bar<T>(elm: One, t: T) -> T
}

struct S<P: Proto> {
    let x: P.Two
    func foo<T>(_ t: T) -> T where P.Two == P.One {
        let x: P.Two = self.x
        let elm: P.One = x as! P.One
        return P.bar(elm: elm, t: t)
    }
}
