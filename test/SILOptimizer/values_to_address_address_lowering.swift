// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -Xllvm -sil-print-types -enable-sil-opaque-values -enable-experimental-feature Lifetimes -enable-experimental-feature AddressableParameters -parse-as-library -module-name values_to_address %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -enable-sil-opaque-values -enable-experimental-feature Lifetimes -enable-experimental-feature AddressableParameters -parse-as-library -module-name values_to_address %s -o /dev/null

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableParameters

// Under opaque values an addressable parameter is a by-value object until
// AddressLowering. SILGen asks for its address with value_to_address, which
// AddressLowering resolves to the address the caller passed. A lifetime
// dependence on the parameter therefore depends on the caller's own storage,
// not on a local copy.

struct NE: ~Escapable {
  var p: UnsafeRawPointer?
  @_lifetime(immortal) init() { p = nil }
}

@_lifetime(borrow x)
func getNE<T>(_ x: borrowing @_addressable T) -> NE { NE() }

// CHECK-LABEL: sil hidden @$s{{.*}}1fyAA2NEVxlF : $@convention(thin) <T> (@in_guaranteed T) -> @lifetime(borrow address 0) @owned NE {
// CHECK: bb0(%0 : @noImplicitCopy $*T):
// CHECK-NOT:     alloc_stack
// CHECK-NOT:     value_to_address
// CHECK:         [[RESULT:%.*]] = apply {{%.*}}<T>(%0) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @lifetime(borrow address 0) @owned NE
// CHECK-NEXT:    mark_dependence [nonescaping] [[RESULT]] : $NE on %0 : $*T
// CHECK-LABEL: } // end sil function '$s{{.*}}1fyAA2NEVxlF'
@_lifetime(borrow x)
func f<T>(_ x: borrowing @_addressable T) -> NE { getNE(x) }
