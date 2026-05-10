extension Foo {
    init(_ bar: Bar)
        self.init

// https://github.com/apple/swift/issues/53340

// RUN: %sourcekitd-test -req=cursor -offset=51 %s -- %s
