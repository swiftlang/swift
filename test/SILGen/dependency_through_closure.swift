// RUN: %target-swift-emit-silgen -verify -enable-experimental-feature Lifetimes -enable-experimental-feature AddressableTypes %s

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableTypes

@_addressableForDependencies
struct Owner {
    @_lifetime(borrow self)
    func reference() -> Reference { fatalError() }
}

struct Reference: ~Escapable {
    @_lifetime(immortal)
    init() { fatalError() }

    func use() {}
}

func closure(_: () -> Void) {}

func dependencyThroughClosure(from owner: borrowing Owner) {
    closure { owner.reference().use() }
}
