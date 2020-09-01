// RUN: %target-swift-frontend  -primary-file %s -O -sil-verify-all -module-name=test -emit-sil | %FileCheck %s

// Also do an end-to-end test to check if the generated code is correct.
// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -Xllvm -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT
// REQUIRES: executable_test

var g = 0

final class Myclass {
  lazy var lvar: Int = {
    print("lvar init")
    return g
  }()
}

struct Mystruct {
  lazy var lvar: Int = {
    print("lvar init")
    return g
  }()
}

// CHECK-LABEL: sil {{.*}} @$s4test0A7_simpleySiAA7MyclassCF
// CHECK:   [[GETTER:%[0-9]+]] = function_ref @$s4test7MyclassC4lvarSivg
// CHECK:   [[V1:%[0-9]+]] = apply [[GETTER]](%0)
// CHECK:   [[V2OPT:%[0-9]+]] = load
// CHECK:   [[V2:%[0-9]+]] = unchecked_enum_data [[V2OPT]]
// CHECK:   [[V1VAL:%[0-9]+]] = struct_extract [[V1]]
// CHECK:   [[V2VAL:%[0-9]+]] = struct_extract [[V2]]
// CHECK:   builtin "sadd{{.*}}"([[V1VAL]] {{.*}}, [[V2VAL]]
// CHECK: } // end sil function '$s4test0A7_simpleySiAA7MyclassCF'
@inline(never)
func test_simple(_ c: Myclass) -> Int {
  g = 27
  let v1 = c.lvar
  let v2 = c.lvar
  return v1 &+ v2
}

// CHECK-LABEL: sil {{.*}} @$s4test0A4_cfgySiAA7MyclassC_SbtF
// CHECK:   [[GETTER:%[0-9]+]] = function_ref @$s4test7MyclassC4lvarSivg
// CHECK:   [[V1:%[0-9]+]] = apply [[GETTER]](%0)
// CHECK: bb1:
// CHECK:   [[V2OPT:%[0-9]+]] = load
// CHECK:   [[V2:%[0-9]+]] = unchecked_enum_data [[V2OPT]]
// CHECK:   [[V2VAL:%[0-9]+]] = struct_extract [[V2]]
// CHECK:   br bb3([[V2VAL]]
// CHECK: bb2:
// CHECK: bb3([[V2PHI:%[0-9]+]] {{.*}}):
// CHECK:   [[V1VAL:%[0-9]+]] = struct_extract [[V1]]
// CHECK:   builtin "sadd{{.*}}"([[V1VAL]] {{.*}}, [[V2PHI]]
// CHECK: } // end sil function '$s4test0A4_cfgySiAA7MyclassC_SbtF'
@inline(never)
func test_cfg(_ c: Myclass, _ b: Bool) -> Int {
  g = 10
  let v1 = c.lvar
  var v2: Int
  if b {
    v2 = c.lvar
  } else {
    v2 = 0
  }
  return v1 &+ v2
}

// CHECK-LABEL: sil {{.*}} @$s4test0A12_no_hoistingySiAA7MyclassC_SbtF
// CHECK: bb1:
// CHECK:   [[GETTER1:%[0-9]+]] = function_ref @$s4test7MyclassC4lvarSivg
// CHECK:    = apply [[GETTER1]](%0)
// CHECK: bb2:
// CHECK:   [[GETTER2:%[0-9]+]] = function_ref @$s4test7MyclassC4lvarSivg
// CHECK:    = apply [[GETTER2]](%0)
// CHECK: bb3({{.*}}):
// CHECK: } // end sil function '$s4test0A12_no_hoistingySiAA7MyclassC_SbtF'
@inline(never)
func test_no_hoisting(_ c: Myclass, _ b: Bool) -> Int {
  var v: Int
  if b {
    g = 20
    v = c.lvar + 2
  } else {
    g = 30
    v = c.lvar + 3
  }
  return v
}

// This test is disabled, because for structs, it does not work yet.
// CSE is too conservative to handle indirect getter arguments currently.

// CHECK-LABEL: sil {{.*}} @$s4test0A7_structySiAA8MystructVF
// CHECK-DISABLED:   [[GETTER:%[0-9]+]] = function_ref @$s4test8MystructV4lvarSivg
// CHECK-DISABLED:   [[V1:%[0-9]+]] = apply [[GETTER]]({{.*}})
// CHECK-DISABLED:   [[V2OPT:%[0-9]+]] = load
// CHECK-DISABLED:   [[V2:%[0-9]+]] = unchecked_enum_data [[V2OPT]]
// CHECK-DISABLED:   [[V1VAL:%[0-9]+]] = struct_extract [[V1]]
// CHECK-DISABLED:   [[V2VAL:%[0-9]+]] = struct_extract [[V2]]
// CHECK-DISABLED:   builtin "sadd{{.*}}"([[V1VAL]] {{.*}}, [[V2VAL]]
// CHECK-DISABLED: } // end sil function '$s4test0A7_structySiAA8MystructVF'
@inline(never)
func test_struct(_ s: Mystruct) -> Int {
  var sm = s
  g = 42
  let v1 = sm.lvar
  let v2 = sm.lvar
  return v1 &+ v2
}

// CHECK-LABEL: sil {{.*}} @$s4test0A19_overwritten_structySiAA8MystructVF
// CHECK:   [[GETTER:%[0-9]+]] = function_ref @$s4test8MystructV4lvarSivg
// CHECK:   [[V1:%[0-9]+]] = apply [[GETTER]]({{.*}})
// CHECK:   [[V2:%[0-9]+]] = apply [[GETTER]]({{.*}})
// CHECK:   [[V1VAL:%[0-9]+]] = struct_extract [[V1]]
// CHECK:   [[V2VAL:%[0-9]+]] = struct_extract [[V2]]
// CHECK:   builtin "sadd{{.*}}"([[V1VAL]] {{.*}}, [[V2VAL]]
// CHECK: } // end sil function '$s4test0A19_overwritten_structySiAA8MystructVF'
@inline(never)
func test_overwritten_struct(_ s: Mystruct) -> Int {
  var sm = s
  g = 42
  let v1 = sm.lvar
  sm = s
  g = 43
  let v2 = sm.lvar
  return v1 &+ v2
}

func calltests() {
  // CHECK-OUTPUT-LABEL: test_simple
  print("test_simple")
  // CHECK-OUTPUT-NEXT:  lvar init
  // CHECK-OUTPUT-NEXT:  54
  print(test_simple(Myclass()))

  // CHECK-OUTPUT-LABEL: test_cfg
  print("test_cfg")
  // CHECK-OUTPUT-NEXT:  lvar init
  // CHECK-OUTPUT-NEXT:  20
  print(test_cfg(Myclass(), true))

  // CHECK-OUTPUT-LABEL: test_no_hoisting
  print("test_no_hoisting")
  // CHECK-OUTPUT-NEXT:  lvar init
  // CHECK-OUTPUT-NEXT:  22
  print(test_no_hoisting(Myclass(), true))
  // CHECK-OUTPUT-NEXT:  lvar init
  // CHECK-OUTPUT-NEXT:  33
  print(test_no_hoisting(Myclass(), false))

  // CHECK-OUTPUT-LABEL: test_struct
  print("test_struct")
  // CHECK-OUTPUT-NEXT:  lvar init
  // CHECK-OUTPUT-NEXT:  84
  print(test_struct(Mystruct()))

  // CHECK-OUTPUT-LABEL: test_overwritten_struct
  print("test_overwritten_struct")
  // CHECK-OUTPUT-NEXT:  lvar init
  // CHECK-OUTPUT-NEXT:  lvar init
  // CHECK-OUTPUT-NEXT:  85
  print(test_overwritten_struct(Mystruct()))
}

calltests()

