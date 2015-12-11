// RUN: not %target-swift-frontend %s -parse

// rdar://22007370

class Foo {
    subscript(key: String) -> String {
        get { a }
        set { b }
    }

    subscript(key: String) -> String {
        get { a }
        set { b }
    }
}
