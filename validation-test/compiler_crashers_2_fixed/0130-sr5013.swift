// RUN: not %target-swift-frontend -typecheck %s


protocol A {
    associatedtype B
}

extension A {
    func foo() {
        (B.self as! Sequence.Type).Element
    }
}
