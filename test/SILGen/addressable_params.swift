// RUN: %target-swift-emit-silgen -enable-experimental-feature AddressableParameters %s | %FileCheck %s

// REQUIRES: swift_feature_AddressableParameters

// CHECK-LABEL: sil {{.*}}@$s{{.*}}6withUP{{.*}} : $@convention(thin) <T> (@in_guaranteed T, @guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> (UnsafePointer<τ_0_0>) -> () for <T>) -> ()
func withUP<T>(to: borrowing @_addressable T, _ body: (UnsafePointer<T>) -> Void) -> Void {}

// Forwarding an addressable parameter binding as an argument to another
// addressable parameter with matching ownership forwards the address without
// copying or moving the value.
// CHECK-LABEL: sil {{.*}}@$s{{.*}}14testForwarding{{.*}} :
// CHECK-SAME:    $@convention(thin) (@in_guaranteed String) -> ()
func testForwarding(x: borrowing @_addressable String) {
    // CHECK: [[MO:%.*]] = copyable_to_moveonlywrapper_addr %0
    // CHECK: [[MOR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[MO]]
    // CHECK: [[MORC:%.*]] = moveonlywrapper_to_copyable_addr [[MOR]]
    // CHECK: apply {{.*}}([[MORC]],
    withUP(to: x) {
        _ = $0
    }
}

func normalBorrowingArgument(_: borrowing String) {}
func normalConsumingArgument(_: consuming String) {}

func normalGenericArgument<T>(_: borrowing T) {}

func testUseAsNormalArgument(x: borrowing @_addressable String) {
    normalBorrowingArgument(x)
    normalConsumingArgument(copy x)
    normalGenericArgument(x)
}

struct Foo {
    var x: String

    // CHECK-LABEL: sil {{.*}}@$s{{.*}}18testForwardingSelf{{.*}} :
    // CHECK-SAME:    $@convention(method) (@in_guaranteed Foo) -> ()
    @_addressableSelf borrowing func testForwardingSelf() {
        // CHECK: [[MO:%.*]] = copyable_to_moveonlywrapper_addr %0
        // CHECK: [[MOR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[MO]]
        // CHECK: [[MORC:%.*]] = moveonlywrapper_to_copyable_addr [[MOR]]
        // CHECK: apply {{.*}}([[MORC]],
        withUP(to: self) {
            _ = $0
        }
    }
}

enum TestEnum {
    case foo(String)
    case bar(String)
}

func addressableParam(_: @_addressable String) -> Bool { true }

func testAddressableSwitchBinding(e: TestEnum) -> Bool {
    return switch e {
    case .foo(let f) where addressableParam(f):
        true
    case .bar(let b):
        addressableParam(b)
    default:
        false
    }
}
