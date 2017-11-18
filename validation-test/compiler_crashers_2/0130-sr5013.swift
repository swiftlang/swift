// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: asserts

protocol A {
    associatedtype B
}

extension A {
    func foo() {
        (B.self as! Sequence.Type).Element
    }
}
