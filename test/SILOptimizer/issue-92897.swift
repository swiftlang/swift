// RUN: %target-swift-frontend -emit-sil -O %s

// https://github.com/swiftlang/swift/issues/92897

public protocol P: ~Copyable {}
public struct S: P {}

public func dynamicCast<U>(_ type: Any.Type, to _: U.Type) -> U? { type as? U }

@inline(never) public func sink(_ t: (any P.Type)?) {}

sink(dynamicCast(S.self, to: (any P.Type).self))
