// RUN: %target-swift-frontend -emit-ir %s

// https://github.com/apple/swift/issues/56247

public protocol Prot {
    associatedtype T
}

public class C<P: Prot> where P.T: Hashable {
}

public class Spam<P: Prot> where P.T == Int {
    public func m(_ f: (C<P>) -> C<P>) {
    }
}
