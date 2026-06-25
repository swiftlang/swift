// RUN: %target-swift-frontend -emit-ir %s -g -O -parse-as-library -module-name a

// rdar://179749587
// Make sure inlining a function with a parameter pack does not produce
// conflicting debug variable scopes.

@_alwaysEmitIntoClient
@inline(always)
public func build<each C>(_ content: repeat each C) -> (repeat each C) {
    (repeat each content)
}

@inlinable
public func test() -> (Int, Int) {
    build(1, 2)
}
