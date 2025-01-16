// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature BuiltinModule -enable-experimental-feature LifetimeDependence -enable-experimental-feature AddressableTypes -enable-experimental-feature ValueGenerics %s | %FileCheck %s

// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_ValueGenerics

import Builtin

// Copied from the stdlib until we have Builtin.overrideLifetime.
@_unsafeNonescapableResult
@lifetime(borrow source)
internal func _overrideLifetime<T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable>(
  _ dependent: consuming T, borrowing source: borrowing U) -> T {
  dependent
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

    // CHECK-LABEL: sil {{.*}}@${{.*}}7NodeRefV9allocated{{.*}}fC :
    // CHECK-SAME:    (@guaranteed AllocatedNode,
    @lifetime(borrow allocated)
    init(allocated: borrowing AllocatedNode) {
        self.parent = allocated.node
    }
}

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

// not addressable for dependencies
struct AllocatedNode: ~Copyable {
    fileprivate var node: UnsafePointer<Node>

    var ref: NodeRef {
      // CHECK-LABEL: sil {{.*}}@${{.*}}13AllocatedNodeV3ref{{.*}}Vvg :
      // CHECK-SAME:    (@guaranteed AllocatedNode) ->
      @lifetime(borrow self)
      borrowing get {
        return NodeRef(allocated: self)
      }
    }
}

struct Schmector {
    // structurally addressable-for-dependencies by virtue of containing a
    // Builtin.FixedArray
    private var elements: Builtin.FixedArray<10, Int>

    var storage: Spam {
        // CHECK-LABEL: sil {{.*}}@${{.*}}9SchmectorV7storage{{.*}}Vvg :
        // CHECK-SAME:    (@in_guaranteed Schmector) ->
        @lifetime(borrow self)
        borrowing get {
            let pointer = UnsafePointer<Int>(Builtin.addressOfBorrow(self))
            let spam = Spam(base: pointer, count: 10)
            return _overrideLifetime(spam, borrowing: self)
        }
    }
}

struct Spam: ~Escapable {
    @lifetime(borrow base)
    init(base: UnsafePointer<Int>, count: Int) {}
}
