// RUN: %target-swift-emit-silgen -verify -enable-experimental-feature LifetimeDependence -enable-experimental-feature AddressableTypes %s

// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_AddressableTypes

@_addressableForDependencies
struct Owner {
    @lifetime(borrow self)
    func reference() -> Reference { fatalError() }
}

struct Reference: ~Escapable {
    @lifetime(immortal)
    init() { fatalError() }

    func use() {}
}

func closure(_: () -> Void) {}

func dependencyThroughClosure(from owner: borrowing Owner) {
    closure { owner.reference().use() }
}
