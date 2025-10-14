// RUN:%target-swift-frontend -emit-silgen %s -enable-experimental-feature BorrowAndMutateAccessors -enable-sil-opaque-values | %FileCheck %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors

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

