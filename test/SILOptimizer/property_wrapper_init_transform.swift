// RUN: %target-swift-frontend -module-name test -disable-availability-checking -emit-sil %s 2>&1 | %FileCheck %s

// REQUIRES: asserts

@propertyWrapper
public struct Wrapper {
  public var wrappedValue: Int { 0 }
  public init(wrappedValue: Int) {}
}

struct S {
  // CHECK-LABEL: sil hidden @$s4test1SVACycfC : $@convention(method) (@thin S.Type) -> S
  // CHECK: [[BACKING_INIT:%.*]] = function_ref @$s4test1SV5valueSivpfP : $@convention(thin) (Int) -> Wrapper
  // CHECK-NEXT: [[WRAPPER_VALUE:%.*]] = apply [[BACKING_INIT]]({{.*}}) : $@convention(thin) (Int) -> Wrapper
  // CHECK-NEXT: [[BACKING_VAR:%.*]] = struct_element_addr {{.*}} : $*S, #S._value
  // CHECK-NEXT: store [[WRAPPER_VALUE]] to [[BACKING_VAR]] : $*Wrapper
  @Wrapper let value: Int
  
  init() {
    value = 10
  }
}

@propertyWrapper
public struct WrapperStructWithGenericType<T> {
  public var wrappedValue: T
  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

struct R {
  // CHECK-LABEL: sil hidden @$s4test1RV1vACSi_tcfC : $@convention(method) (Int, @thin R.Type) -> R {
  // CHECK: [[BACKING_INIT:%.*]] = function_ref @$s4test1RV5valueSivpfP : $@convention(thin) (Int) -> WrapperStructWithGenericType<Int>
  // CHECK: [[WRAPPER_VALUE:%.*]] = apply [[BACKING_INIT]]({{.*}}) : $@convention(thin) (Int) -> WrapperStructWithGenericType<Int>
  // CHECK: [[BACKING_VAR:%.*]] = struct_element_addr {{.*}} : $*R, #R._value
  // CHECK: store [[WRAPPER_VALUE]] to [[BACKING_VAR]] : $*WrapperStructWithGenericType<Int>
  @WrapperStructWithGenericType let value: Int
  
  init(v: Int) {
    value = v
  }
}

let r = R(v: 10)

@propertyWrapper
public class WrapperClassWithGenericType<T> {
  public var wrappedValue: T
  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

class C {
  // CHECK-LABEL: sil hidden @$s4test1CC1vACSi_tcfc : $@convention(method) (Int, @owned C) -> @owned C {
  // CHECK:     [[BACKING_INIT:%.*]] = function_ref @$s4test1CC5valueSivpfP : $@convention(thin) (Int) -> @owned WrapperClassWithGenericType<Int>
  // CHECK:     [[WRAPPER_VALUE:%.*]] = apply [[BACKING_INIT]]({{.*}}) : $@convention(thin) (Int) -> @owned WrapperClassWithGenericType<Int>
  // CHECK:     [[BACKING_VAR:%.*]] = ref_element_addr {{.*}} : $C, #C._value
  // CHECK:     store [[WRAPPER_VALUE]] to [[BACKING_VAR]] : $*WrapperClassWithGenericType<Int>
  @WrapperClassWithGenericType let value: Int
  
  init(v: Int) {
    value = v
  }
}

let c = C(v: 10)
