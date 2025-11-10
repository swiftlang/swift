// RUN: %target-swift-frontend %s -emit-ir

// https://github.com/apple/swift/issues/43445

public class A <T> {
    public init() {}
}

public class B : A <B.C> {
    public struct C { }
    public override init() {}
}
