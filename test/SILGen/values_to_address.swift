// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-sil-opaque-values -enable-experimental-feature AddressableParameters -parse-as-library -module-name values_to_address %s | %FileCheck %s

// REQUIRES: swift_feature_AddressableParameters

// With opaque values, a guaranteed address-only argument is an SSA value, so it
// has no address in SIL until AddressLowering. The address that an addressable
// parameter promises is the address of the argument itself, which SILGen asks
// for with value_to_address. Building a temporary and borrowing the argument
// into it would give a different address.

func take<T>(_ x: borrowing @_addressable T) {}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s{{.*}}8argument{{.*}} : $@convention(thin) <T> (@in_guaranteed T) -> () {
// CHECK: bb0(%0 : @noImplicitCopy @guaranteed $T):
// CHECK-NOT:     alloc_stack
// CHECK-NOT:     store_borrow
// CHECK:         [[ADDR:%.*]] = value_to_address %0 : $T
// CHECK:         [[WRAPPED:%.*]] = copyable_to_moveonlywrapper_addr [[ADDR]] : $*T
// CHECK:         [[CHECKED:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[WRAPPED]] : $*@moveOnly T
// CHECK:         [[LOAD:%.*]] = load_borrow [[CHECKED]] : $*@moveOnly T
// CHECK:         apply {{%.*}}<T>({{%.*}}) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> ()
// CHECK-NOT:     store_borrow
// CHECK-LABEL: } // end sil function '$s{{.*}}8argument{{.*}}'
func argument<T>(_ x: borrowing @_addressable T) {
  take(x)
}

// A temporary has no stable address to promise, so it isn't an operand of
// value_to_address.
//
// CHECK-LABEL: sil hidden [ossa] [opaque] @$s{{.*}}6rvalue{{.*}} :
// CHECK-NOT:     value_to_address
// CHECK-LABEL: } // end sil function '$s{{.*}}6rvalue{{.*}}'
func rvalue<T>(_ make: () -> T) {
  take(make())
}
