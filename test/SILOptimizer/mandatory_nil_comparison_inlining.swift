// RUN: %target-swift-frontend -enable-sil-ownership -sil-verify-all -primary-file %s -module-name=test -emit-sil -o - -verify | %FileCheck %s

struct X { }

// CHECK-LABEL: sil {{.*}} @{{.*}}generic_func
// CHECK: switch_enum_addr
// CHECK: return
func generic_func<T>(x: [T]?) -> Bool {
  return x == nil
}

// CHECK-LABEL: sil {{.*}} @{{.*}}array_func_rhs_nil
// CHECK: switch_enum_addr
// CHECK: return
func array_func_rhs_nil(x: [X]?) -> Bool {
  return x == nil
}

// CHECK-LABEL: sil {{.*}} @{{.*}}array_func_lhs_nil
// CHECK: switch_enum_addr
// CHECK: return
func array_func_lhs_nil(x: [X]?) -> Bool {
  return nil == x
}

// CHECK-LABEL: sil {{.*}} @{{.*}}array_func_rhs_non_nil
// CHECK: switch_enum_addr
// CHECK: return
func array_func_rhs_non_nil(x: [X]?) -> Bool {
  return x != nil
}

// CHECK-LABEL: sil {{.*}} @{{.*}}array_func_lhs_non_nil
// CHECK: switch_enum_addr
// CHECK: return
func array_func_lhs_non_nil(x: [X]?) -> Bool {
  return nil != x
}

