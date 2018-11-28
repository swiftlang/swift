// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

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

// CHECK: sil hidden @$s7if_expr19addr_only_ternary_1{{[_0-9a-zA-Z]*}}F
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
  // CHECK:   [[READa:%.*]] = begin_access [read] [unknown] [[PBa]]
  // CHECK:   copy_addr [[READa]] to [initialization] [[RET]]
  // CHECK:   br [[CONT:bb[0-9]+]]
  // CHECK: [[FALSE]]:
  // CHECK:   [[READb:%.*]] = begin_access [read] [unknown] [[PBb]]
  // CHECK:   copy_addr [[READb]] to [initialization] [[RET]]
  // CHECK:   br [[CONT]]
  return x ? a : b
}

// <rdar://problem/31595572> - crash when conditional expression is an
// lvalue of IUO type

// CHECK-LABEL: sil hidden @$s7if_expr18iuo_lvalue_ternary1xSiSbSgz_tF : $@convention(thin) (@inout Optional<Bool>) -> Int
// CHECK: [[IUO_BOOL_ADDR:%.*]] = begin_access [read] [unknown] %0 : $*Optional<Bool>
// CHECK: [[IUO_BOOL:%.*]] = load [trivial] [[IUO_BOOL_ADDR]] : $*Optional<Bool>
// CHECK: switch_enum [[IUO_BOOL]]
func iuo_lvalue_ternary(x: inout Bool!) -> Int {
  return x ? 1 : 0
}
