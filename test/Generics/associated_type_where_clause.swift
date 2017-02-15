// RUN: %target-typecheck-verify-swift -swift-version 4

protocol Foo {}
extension Int: Foo {}

protocol Bar1 {
    associatedtype T where T: Foo
}
struct B1: Bar1 {
    typealias T = Int
}

protocol Bar2 {
    associatedtype T where T: Bar1, T.T == Int
}

protocol Bar3 {
    associatedtype T: Bar1 = B1 where T.T == Int
}
