// RUN: %target-swift-frontend -typecheck -verify %s

// rdar://problem/29954938 -- A bug in associated type inference exposed an
// order dependency where, if a type conformed to Collection in one extension
// then conformed to MutableCollection in a later extension, it would fail
// to type-check. This regression test ensures that a "working" order,
// where MutableCollection is the only explicit conformance, still works.

struct Butz { }

extension Butz: MutableCollection {
    public var startIndex: Int { return 0 }
    public var endIndex: Int { return 0 }
}

extension Butz {
    public subscript (_ position: Int) -> Int {
        get { return 0 }
        set {  }
    }
    public func index(after i: Int) -> Int { return 0 }
}
