// RUN: %target-swift-frontend -disable-availability-checking -emit-silgen -verify %s

public func foo() -> some Any { return 1 }

public struct XY<X, Y> { public init(x: X, y: Y) { fatalError() } }

@inlinable
public func bar() -> () -> Any {
    let xy = XY(x: 1, y: foo())

    return { xy }
}
