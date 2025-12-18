 // RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/55444

public final class Foo {
    public var a: String {
        return "\(backgroundContext)"
