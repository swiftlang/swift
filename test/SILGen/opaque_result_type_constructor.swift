// RUN: %target-swift-emit-silgen %s

// https://github.com/swiftlang/swift/issues/68277

enum E: P {
    static func m() -> some Any { () }
}

protocol P {
    associatedtype A
    static func m() -> A
}

struct S<T> {
    let t: T
}

extension S where T == E.A {
    init() {
        self.init(t: E.m())
    }
}
