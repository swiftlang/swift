 // RUN: not %target-swift-frontend -typecheck %s

public final class Foo {
    public var a: String {
        return "\(backgroundContext)"
