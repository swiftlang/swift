// Tests for keypath emission under -enable-sil-opaque-values.

// RUN: %target-swift-emit-silgen -enable-sil-opaque-values %s | %FileCheck --check-prefix=SILGEN %s
// RUN: %target-swift-emit-sil    -enable-sil-opaque-values %s | %FileCheck --check-prefix=SIL    %s

// RUN: %target-swift-frontend -c -enable-sil-opaque-values -sil-verify-all %s -o %t.o

struct Loadable {
  subscript(_ i: Int) -> Int { return i }
}

struct Multi {
  subscript(i: Int, j: String) -> Int { return i }
}

struct Generic<T: Hashable> {
  subscript(_ x: T) -> Int { return 0 }
}

public class PublicClass {
  public var publicProp: Int = 0
}

// The synthesized equals helper takes the indices as opaque object values.

// SILGEN-LABEL: sil shared [thunk] [ossa] @$sSiTH : $@convention(keypath_accessor_equals) (@in_guaranteed Int, @in_guaranteed Int) -> Bool {
// SILGEN:       bb0(%0 : $Int, %1 : $Int):
// SILGEN-NOT:     load
// SILGEN-NOT:     tuple_element_addr
// SILGEN:         apply {{.*}}(%0, %1, {{.*}}) : $@convention(witness_method: Equatable)

// SILGEN-LABEL: sil shared [thunk] [ossa] @$sSiTh : $@convention(keypath_accessor_hash) (@in_guaranteed Int) -> Int {
// SILGEN:       bb0(%0 : $Int):

func mkLoadable() -> KeyPath<Loadable, Int> { return \Loadable.[42] }

// SILGen records the address form of the index LoweredType in the pattern
// even when getLoweredType returns the object form, so the keypath runtime ABI
// sees an address-form index slot.

// SILGEN-LABEL: sil shared [thunk] [ossa] @$sSiSSTH : $@convention(keypath_accessor_equals) (@in_guaranteed (Int, String), @in_guaranteed (Int, String)) -> Bool {
// SILGEN:       bb0(%0 : @guaranteed $(Int, String), %1 : @guaranteed $(Int, String)):
// SILGEN-NOT:     tuple_element_addr
// SILGEN:         tuple_extract %0, 0
// SILGEN:         tuple_extract %1, 0
// SILGEN:         tuple_extract %0, 1
// SILGEN:         tuple_extract %1, 1

// SILGEN-LABEL: sil shared [thunk] [ossa] @$sSiSSTh : $@convention(keypath_accessor_hash) (@in_guaranteed (Int, String)) -> Int {
// SILGEN:       bb0(%0 : @guaranteed $(Int, String)):
// SILGEN-NOT:     tuple_element_addr
// SILGEN:         tuple_extract %0, 0

// Getter helper for multi-index also extracts opaque indices via tuple_extract.

// SILGEN-LABEL: sil shared [thunk] [ossa] @$s{{[^ ]+}}5MultiVyS2i_SStcipACTK :
// SILGEN:       bb0(%0 : $Multi, %1 : @guaranteed $(Int, String)):
// SILGEN-NOT:     tuple_element_addr
// SILGEN:         tuple_extract %1, 0
// SILGEN:         tuple_extract %1, 1

func mkMulti() -> KeyPath<Multi, Int> { return \Multi.[0, "x"] }

// SILVerifier accepts the intermediate state where an index operand is at
// object type while its pattern slot is at address type.

// SILGEN-LABEL: sil hidden [ossa] @$s{{[^ ]+}}9mkGeneric{{[^ ]*}} : $@convention(thin) <T where T : Hashable> (@in_guaranteed T) -> @owned KeyPath<Generic<T>, Int> {
// SILGEN:         keypath $KeyPath<Generic<T>, Int>{{.*}}indices [%$0 : $τ_0_0 : $*τ_0_0]
// SIL-LABEL: sil hidden @$s{{[^ ]+}}9mkGeneric{{[^ ]*}} : $@convention(thin) <T where T : Hashable> (@in_guaranteed T) -> @owned KeyPath<Generic<T>, Int> {
// SIL:         [[STK:%[0-9]+]] = alloc_stack $T
// SIL:         keypath $KeyPath<Generic<T>, Int>{{.*}}indices [%$0 : $τ_0_0 : $*τ_0_0]{{.*}}([[STK]])

func mkGeneric<T: Hashable>(_ x: T) -> KeyPath<Generic<T>, Int> {
  return \Generic<T>.[x]
}

// Public stored properties get a sil_property descriptor under OV mode.

// SILGEN: sil_property #PublicClass.publicProp
