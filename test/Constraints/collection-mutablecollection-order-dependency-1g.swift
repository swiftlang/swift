// RUN: %target-typecheck-verify-swift

// rdar://problem/29954938 -- A bug in associated type inference exposed an
// order dependency where, if a type conformed to Collection in one extension
// then conformed to MutableCollection in a later extension, it would fail
// to type-check.

struct Butz<Flubz: Comparable> { }

extension Butz: Collection {
    public var startIndex: Flubz { fatalError() }
    public var endIndex: Flubz { fatalError() }
}

extension Butz: MutableCollection {
    public subscript (_ position: Flubz) -> Flubz {
        get { fatalError() }
        set {  }
    }
    public func index(after i: Flubz) -> Flubz { fatalError() }
}
