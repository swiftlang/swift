extension Foo {
    init(_ bar: Bar)
        self.init

// RUN: %sourcekitd-test -req=cursor -offset=51 %s -- %s
