// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature BuiltinModule -enable-experimental-feature LifetimeDependence -enable-experimental-feature AddressableTypes -enable-experimental-feature AddressableParameters -verify %s

// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_AddressableParameters
// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_LifetimeDependence

@_addressableForDependencies
struct Node {
    var id: AnyObject

    func grungle() {}
}

struct NodeRef: ~Escapable {
    private var parent: UnsafePointer<Node>

    @lifetime(borrow node)
    init(node: borrowing Node) { fatalError() }
}

// Ensure there aren't spurious errors about consumption when an addressable
// parameter is passed as a normal loadable parameter to another function
// or method.
@lifetime(borrow node)
func test(node: borrowing Node) -> NodeRef {
    node.grungle()
    return NodeRef(node: node)
}

