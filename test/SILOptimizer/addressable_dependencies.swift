// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature BuiltinModule -enable-experimental-feature LifetimeDependence -enable-experimental-feature AddressableTypes %s | %FileCheck %s

// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_LifetimeDependence

import Builtin

@_addressableForDependencies
struct Node {
    var id: String

    var ref: NodeRef {
      // CHECK-LABEL: sil {{.*}}@${{.*}}4NodeV3ref{{.*}}Vvg :
      // CHECK-SAME:    (@in_guaranteed Node) ->
      @lifetime(borrow self)
      borrowing get {
        // CHECK: bb0(%0 : @noImplicitCopy $*Node):
        // CHECK: [[REF:%.*]] = apply {{.*}}(%0,
        // CHECK: mark_dependence [nonescaping] [[REF]] on %0
        return NodeRef(node: self)
      }
    }
}

struct NodeRef: ~Escapable {
    private var parent: UnsafePointer<Node>

    // CHECK-LABEL: sil {{.*}}@${{.*}}7NodeRefV4node{{.*}}fC :
    // CHECK-SAME:    (@in_guaranteed Node,
    @lifetime(borrow node)
    init(node: borrowing Node) {
        // CHECK: bb0(%0 : @noImplicitCopy $*Node,
        // CHECK:   [[RAW_PTR:%.*]] = address_to_pointer {{.*}}%0
        // CHECK:   struct $UnsafePointer<Node> ([[RAW_PTR]])
        self.parent = UnsafePointer(Builtin.addressOfBorrow(node))
    }
}
