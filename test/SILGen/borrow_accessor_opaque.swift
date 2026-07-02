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

// CHECK: sil hidden [ossa] @$s22borrow_accessor_opaque7WrapperV1kAA5KlassCvz : $@convention(method) (@inout Wrapper) -> @inout Klass {
// CHECK: bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #Wrapper._k
// CHECK:   return [[REG2]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s22borrow_accessor_opaque7WrapperVyAA5KlassCSiciz : $@convention(method) (Int, @inout Wrapper) -> @inout Klass {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*Wrapper):
// CHECK:   [[REG4:%.*]] = struct_element_addr [[REG1]], #Wrapper._k
// CHECK:   return [[REG4]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s22borrow_accessor_opaque13SimpleWrapperV4propxvb : $@convention(method) <T> (@in_guaranteed SimpleWrapper<T>) -> @guaranteed_address T {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $SimpleWrapper<T>):
// CHECK:   [[REG2:%.*]] = struct_extract [[REG0]], #SimpleWrapper._prop
// CHECK:   return [[REG2]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s22borrow_accessor_opaque13SimpleWrapperV4propxvz : $@convention(method) <T> (@inout SimpleWrapper<T>) -> @inout T {
// CHECK: bb0([[REG0:%.*]] : $*SimpleWrapper<T>):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #SimpleWrapper._prop
// CHECK:   return [[REG2]]
// CHECK: }

func useValue<T>(_ t: T) {}

// Calling the address-only borrow accessor off an inout base: the base is an
// address (begin_access), but the accessor's self is a by-value @guaranteed
// object under opaque values, so self is load_borrow'd and the @guaranteed_address
// result is copied out.
// CHECK-LABEL: sil hidden [ossa] @$s22borrow_accessor_opaque16readGenericInoutyyAA13SimpleWrapperVyxGzlF : $@convention(thin) <T> (@inout SimpleWrapper<T>) -> () {
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
// CHECK-LABEL: sil hidden [ossa] @$s22borrow_accessor_opaque19readGenericBorrowedyyAA13SimpleWrapperVyxGlF : $@convention(thin) <T> (@in_guaranteed SimpleWrapper<T>) -> () {
// CHECK: bb0([[W:%.*]] : @noImplicitCopy @guaranteed $SimpleWrapper<T>):
// CHECK:   [[FN:%.*]] = function_ref @$s22borrow_accessor_opaque13SimpleWrapperV4propxvb : $@convention(method) <τ_0_0> (@in_guaranteed SimpleWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[RESULT:%.*]] = apply [[FN]]<T>([[W]]) : $@convention(method) <τ_0_0> (@in_guaranteed SimpleWrapper<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[COPY:%.*]] = copy_value [[RESULT]]
// CHECK: }
func readGenericBorrowed<T>(_ w: borrowing SimpleWrapper<T>) {
  useValue(w.prop)
}

