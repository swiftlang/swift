// RUN: %target-swift-frontend -typecheck -verify %s

// rdar://problem/29954938 -- A bug in associated type inference exposed an
// order dependency where, if a type conformed to Collection in one extension
// then conformed to MutableCollection in a later extension, it would fail
// to type-check.

struct Butz { }

extension Butz: Collection {
    public var startIndex: Int { return 0 }
    public var endIndex: Int { return 0 }
}

extension Butz: MutableCollection {
    public subscript (_ position: Int) -> Int {
        get { return 0 }
        set {  }
    }
    public func index(after i: Int) -> Int { return 0 }
}
