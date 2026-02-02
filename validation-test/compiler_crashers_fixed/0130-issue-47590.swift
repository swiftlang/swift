// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/47590

protocol A {
    associatedtype B
}

extension A {
    func foo() {
        (B.self as! Sequence.Type).Element
    }
}
