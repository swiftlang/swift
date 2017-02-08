// RUN: %target-swift-frontend %s -emit-ir

public class A <T> {
    public init() {}
}

public class B : A <B.C> {
    public struct C { }
    public override init() {}
}
