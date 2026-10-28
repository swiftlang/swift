// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-sil -sil-verify-all -o /dev/null -enable-sil-opaque-values %s
// RUN:%target-swift-frontend -emit-silgen %s -enable-sil-opaque-values | %FileCheck %s


public final class Klass {}

public struct Wrapper {
  var _k: Klass

  var k: Klass {
    borrow {
      return _k
    }
    mutate {
      return &_k
    }
  }
  subscript(index: Int) -> Klass {
    borrow {
      return _k
    }
    mutate {
      return &_k
    }
  }
}

public struct SimpleWrapper<T> {
  var _prop: T

  var prop: T {
    borrow {
      return _prop
    }
    mutate {
      return &_prop
    }
  }
}

// CHECK: sil hidden [ossa] [opaque] @$s22borrow_accessor_opaque7WrapperV1kAA5KlassCvz : $@convention(method) (@inout Wrapper) -> @inout Klass {
// CHECK: bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #Wrapper._k
// CHECK:   return [[REG2]]
// CHECK: }

// CHECK: sil hidden [ossa] [opaque] @$s22borrow_accessor_opaque7WrapperVyAA5KlassCSiciz : $@convention(method) (Int, @inout Wrapper) -> @inout Klass {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*Wrapper):
// CHECK:   [[REG4:%.*]] = struct_element_addr [[REG1]], #Wrapper._k
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK: sil hidden [ossa] [opaque] @$s22borrow_accessor_opaque13SimpleWrapperV4propxvb : $@convention(method) <T> (@in_guaranteed SimpleWrapper<T>) -> @guaranteed_address T {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $SimpleWrapper<T>):
// CHECK:   [[REG2:%.*]] = struct_extract [[REG0]], #SimpleWrapper._prop
// CHECK:   return [[REG2]]
// CHECK: }

// CHECK: sil hidden [ossa] [opaque] @$s22borrow_accessor_opaque13SimpleWrapperV4propxvz : $@convention(method) <T> (@inout SimpleWrapper<T>) -> @inout T {
// CHECK: bb0([[REG0:%.*]] : $*SimpleWrapper<T>):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #SimpleWrapper._prop
// CHECK:   return [[REG2]]
// CHECK: }

func useValue<T>(_ t: T) {}

// Calling the address-only borrow accessor off an inout base: the base is an
// address (begin_access), but the accessor's self is a by-value @guaranteed
// object under opaque values, so self is load_borrow'd and the @guaranteed_address
// result is copied out.
// CHECK-LABEL: sil hidden [ossa] [opaque] @$s22borrow_accessor_opaque16readGenericInoutyyAA13SimpleWrapperVyxGzlF : $@convention(thin) <T> (@inout SimpleWrapper<T>) -> () {
// CHECK: bb0([[W:%.*]] : $*SimpleWrapper<T>):
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[W]]
// CHECK:   [[FN:%.*]] = function_ref @$s22borrow_accessor_opaque13SimpleWrapperV4propxvb : $@convention(method) <τ_0_0> (@in_guaranteed SimpleWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[SELF:%.*]] = load_borrow [[ACCESS]]
// CHECK:   [[RESULT:%.*]] = apply [[FN]]<T>([[SELF]]) : $@convention(method) <τ_0_0> (@in_guaranteed SimpleWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[COPY:%.*]] = copy_value [[RESULT]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   end_borrow [[SELF]]
// CHECK: }
func readGenericInout<T>(_ w: inout SimpleWrapper<T>) {
  useValue(w.prop)
}

// Calling the address-only borrow accessor off a borrowed base: self is already
// a @guaranteed object, passed directly to the accessor.
// CHECK-LABEL: sil hidden [ossa] [opaque] @$s22borrow_accessor_opaque19readGenericBorrowedyyAA13SimpleWrapperVyxGlF : $@convention(thin) <T> (@in_guaranteed SimpleWrapper<T>) -> () {
// CHECK: bb0([[W:%.*]] : @noImplicitCopy @guaranteed $SimpleWrapper<T>):
// CHECK:   [[FN:%.*]] = function_ref @$s22borrow_accessor_opaque13SimpleWrapperV4propxvb : $@convention(method) <τ_0_0> (@in_guaranteed SimpleWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[RESULT:%.*]] = apply [[FN]]<T>([[W]]) : $@convention(method) <τ_0_0> (@in_guaranteed SimpleWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[COPY:%.*]] = copy_value [[RESULT]]
// CHECK: }
func readGenericBorrowed<T>(_ w: borrowing SimpleWrapper<T>) {
  useValue(w.prop)
}

// Calling the address-only mutate accessor off an inout base: unlike the borrow
// accessor above, a mutating mutate accessor's self is @inout, which is an
// address in every mode, so the begin_access is passed straight through with no
// load and stays mutable at the call site.
// CHECK-LABEL: sil hidden [ossa] [opaque] @$s22borrow_accessor_opaque18mutateGenericInoutyyAA13SimpleWrapperVyxGz_xtlF : $@convention(thin) <T> (@inout SimpleWrapper<T>, @in_guaranteed T) -> () {
// CHECK: bb0([[W:%.*]] : $*SimpleWrapper<T>, [[V:%.*]] : @guaranteed $T):
// CHECK:   [[COPY:%.*]] = copy_value [[V]]
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unknown] [[W]]
// CHECK:   [[FN:%.*]] = function_ref @$s22borrow_accessor_opaque13SimpleWrapperV4propxvz : $@convention(method) <τ_0_0> (@inout SimpleWrapper<τ_0_0>) -> @inout τ_0_0
// CHECK-NOT: load_borrow
// CHECK:   [[RESULT:%.*]] = apply [[FN]]<T>([[ACCESS]]) : $@convention(method) <τ_0_0> (@inout SimpleWrapper<τ_0_0>) -> @inout τ_0_0
// CHECK:   assign [[COPY]] to [[RESULT]]
// CHECK:   end_access [[ACCESS]]
// CHECK: }
func mutateGenericInout<T>(_ w: inout SimpleWrapper<T>, _ v: T) {
  w.prop = v
}

// The two cases below are the converse of `mutateGenericInout`: mutate accessors
// whose self is *not* @inout, and so is a by-value object under opaque values.
// Both are well-formed because their @inout result addresses the pointee, which
// lives outside self, rather than self's own storage.

// On a class, `mutating` isn't available, so self is a @guaranteed class
// reference, an object.
final class PointerBox<T> {
  let _storage: UnsafeMutablePointer<T>
  init(_ s: UnsafeMutablePointer<T>) { _storage = s }
  var value: T {
    @_unsafeSelfDependentResult
    borrow {
      return _storage.pointee
    }
    @_unsafeSelfDependentResult
    mutate {
      return &_storage.pointee
    }
  }
}

// Self is an object ($PointerBox<T>, not $*PointerBox<T>) even though the
// accessor's result is @inout.
// CHECK-LABEL: sil hidden [ossa] [opaque] @$s22borrow_accessor_opaque10PointerBoxC5valuexvz : $@convention(method) <T> (@guaranteed PointerBox<T>) -> @inout T {
// CHECK: bb0({{%.*}} : @guaranteed $PointerBox<T>):

// The base is an address but self is by-value, so it is loaded. Note the access
// is [read], not [modify]: self itself is never mutated. The mutation lands on
// the accessor's @inout result instead.
// CHECK-LABEL: sil hidden [ossa] [opaque] @$s22borrow_accessor_opaque16mutateClassInoutyyAA10PointerBoxCyxGz_xtlF : $@convention(thin) <T> (@inout PointerBox<T>, @in_guaranteed T) -> () {
// CHECK: bb0([[B:%.*]] : $*PointerBox<T>, [[V:%.*]] : @guaranteed $T):
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[B]]
// CHECK:   [[SELF:%.*]] = load [copy] [[ACCESS]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[COPY:%.*]] = copy_value [[V]]
// CHECK:   [[BORROW:%.*]] = begin_borrow [[SELF]]
// CHECK:   [[FN:%.*]] = function_ref @$s22borrow_accessor_opaque10PointerBoxC5valuexvz : $@convention(method) <τ_0_0> (@guaranteed PointerBox<τ_0_0>) -> @inout τ_0_0
// CHECK:   [[RESULT:%.*]] = apply [[FN]]<T>([[BORROW]]) : $@convention(method) <τ_0_0> (@guaranteed PointerBox<τ_0_0>) -> @inout τ_0_0
// CHECK:   assign [[COPY]] to [[RESULT]]
// CHECK:   end_borrow [[BORROW]]
// CHECK: }
func mutateClassInout<T>(_ b: inout PointerBox<T>, _ v: T) {
  b.value = v
}

// An explicit `nonmutating mutate` on a value type: self is @in_guaranteed,
// which opaque values lowers to an object.
struct PointerWrapper<T> {
  let _storage: UnsafeMutablePointer<T>
  var _extra: T                     // makes PointerWrapper<T> address-only
  var value: T {
    @_unsafeSelfDependentResult
    borrow {
      return _storage.pointee
    }
    @_unsafeSelfDependentResult
    nonmutating mutate {
      return &_storage.pointee
    }
  }
}

// The @in_guaranteed self is an object here: SILAddressConventions only treats
// Indirect_In_Guaranteed as an address once addresses have been lowered, so
// under opaque values it is passed by value, unlike Indirect_Inout, which is
// an address unconditionally.
// CHECK-LABEL: sil hidden [ossa] [opaque] @$s22borrow_accessor_opaque14PointerWrapperV5valuexvz : $@convention(method) <T> (@in_guaranteed PointerWrapper<T>) -> @inout T {
// CHECK: bb0({{%.*}} : @guaranteed $PointerWrapper<T>):

// Same shape as the class case above: self loaded and borrowed, [read] access,
// mutation applied to the @inout result.
// CHECK-LABEL: sil hidden [ossa] [opaque] @$s22borrow_accessor_opaque22mutateNonmutatingInoutyyAA14PointerWrapperVyxGz_xtlF : $@convention(thin) <T> (@inout PointerWrapper<T>, @in_guaranteed T) -> () {
// CHECK: bb0([[W:%.*]] : $*PointerWrapper<T>, [[V:%.*]] : @guaranteed $T):
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[W]]
// CHECK:   [[SELF:%.*]] = load [copy] [[ACCESS]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   [[COPY:%.*]] = copy_value [[V]]
// CHECK:   [[BORROW:%.*]] = begin_borrow [[SELF]]
// CHECK:   [[FN:%.*]] = function_ref @$s22borrow_accessor_opaque14PointerWrapperV5valuexvz : $@convention(method) <τ_0_0> (@in_guaranteed PointerWrapper<τ_0_0>) -> @inout τ_0_0
// CHECK:   [[RESULT:%.*]] = apply [[FN]]<T>([[BORROW]]) : $@convention(method) <τ_0_0> (@in_guaranteed PointerWrapper<τ_0_0>) -> @inout τ_0_0
// CHECK:   assign [[COPY]] to [[RESULT]]
// CHECK:   end_borrow [[BORROW]]
// CHECK: }
func mutateNonmutatingInout<T>(_ w: inout PointerWrapper<T>, _ v: T) {
  w.value = v
}

