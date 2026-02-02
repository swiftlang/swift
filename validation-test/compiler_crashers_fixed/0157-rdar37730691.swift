// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -c %s -index-store-path %t

struct X : Z {
    func b(_ : Y) {}
}

protocol Z {
    associatedtype a
    typealias Y = a.c
}
