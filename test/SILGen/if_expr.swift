// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

func fizzbuzz(i: Int) -> String {
  return i % 3 == 0
    ? "fizz"
    : i % 5 == 0
    ? "buzz"
    : "\(i)"
  // CHECK: cond_br {{%.*}}, [[OUTER_TRUE:bb[0-9]+]], [[OUTER_FALSE:bb[0-9]+]]
  // CHECK: [[OUTER_TRUE]]:
  // CHECK: br [[OUTER_CONT:bb[0-9]+]]
  // CHECK: [[OUTER_FALSE]]:
  // CHECK: cond_br {{%.*}}, [[INNER_TRUE:bb[0-9]+]], [[INNER_FALSE:bb[0-9]+]]
  // CHECK: [[INNER_TRUE]]:
  // CHECK: br [[INNER_CONT:bb[0-9]+]]
  // CHECK: [[INNER_FALSE]]:
  // CHECK: function_ref {{.*}}stringInterpolation
  // CHECK: br [[INNER_CONT]]
  // CHECK: [[INNER_CONT]]({{.*}}):
  // CHECK: br [[OUTER_CONT]]
  // CHECK: [[OUTER_CONT]]({{.*}}):
  // CHECK: return
}

protocol AddressOnly {}

struct A : AddressOnly {}
struct B : AddressOnly {}

func consumeAddressOnly(_: AddressOnly) {}

// CHECK: sil hidden @_T07if_expr19addr_only_ternary_1{{[_0-9a-zA-Z]*}}F
func addr_only_ternary_1(x: Bool) -> AddressOnly {
  // CHECK: bb0([[RET:%.*]] : $*AddressOnly, {{.*}}):
  // CHECK: [[a:%[0-9]+]] = alloc_box ${ var AddressOnly }, var, name "a"
  // CHECK: [[PBa:%.*]] = project_box [[a]]
  var a : AddressOnly = A()
  // CHECK: [[b:%[0-9]+]] = alloc_box ${ var AddressOnly }, var, name "b"
  // CHECK: [[PBb:%.*]] = project_box [[b]]
  var b : AddressOnly = B()

  // CHECK:   cond_br {{%.*}}, [[TRUE:bb[0-9]+]], [[FALSE:bb[0-9]+]]
  // CHECK: [[TRUE]]:
  // CHECK:   copy_addr [[PBa]] to [initialization] [[RET]]
  // CHECK:   br [[CONT:bb[0-9]+]]
  // CHECK: [[FALSE]]:
  // CHECK:   copy_addr [[PBb]] to [initialization] [[RET]]
  // CHECK:   br [[CONT]]
  return x ? a : b
}
