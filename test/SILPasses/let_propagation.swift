// RUN: %target-swift-frontend -primary-file %s -disable-func-sig-opts -emit-sil -O | FileCheck %s

// Check that LoadStoreOpts can handle "let" variables properly.
// Such variables should be loaded only once and their loaded values can be reused.
// This is safe, because once assigned, these variables cannot change their value.

// Helper function, which models an external functions with unknown side-effects.
// It is calle just to trigger flushing of all known stored in LoadStore optimizations.
@inline(never)
func action() {
}

final public class A0 {
 let x: Int32 = 3
 let y: Int32 = 1
 
 @inline(never)
 func sum1() -> Int32 {
    // x and y should be loaded only once.
    let n = x + y
    action()
    let m = x - y
    action()
    let p = x - y + 1
    return n + m + p
 }

 func sum2() -> Int32 {
    // x and y should be loaded only once.
    let n = x + y
    action()
    let m = x - y
    action()
    let p = x - y + 1
    return n + m + p
 }
}


// Check that counter computation is completely evaluated
// at compile-time, because the value of a.x and a.y are known
// from the initializer and propagated into their uses, because
// we know that action() invocations do not affect their values.
//
// CHECK-LABEL: sil {{.*}}testAllocAndUseLet
// CHECK: bb0
// CHECK-NOT: ref_element_addr
// CHECK-NOT: struct_element_addr
// CHECK-NOT: bb1
// CHECK: function_ref @_TF15let_propagation6actionFT_T_
// CHECK: apply
// CHECK: apply
// CHECK: apply
// CHECK: apply
// CHECK: apply
// CHECK: apply
// CHECK: apply
// CHECK: apply
// CHECK: integer_literal $Builtin.Int32, 36
// CHECK-NEXT: struct $Int32 ({{.*}} : $Builtin.Int32)
// CHECK-NEXT: return
@inline(never)
public func testAllocAndUseLet() -> Int32 {
  var a = A0()
  var counter: Int32
  // a.x and a.y should be loaded only once.
  counter = a.sum2() + a.sum2()
  counter += a.sum2() + a.sum2()
  return counter
}

// Check that a.x and a.y are loaded only once and then reused.
// CHECK-LABEL: sil {{.*}}testUseLet
// CHECK: bb0
// CHECK: ref_element_addr
// CHECK: struct_element_addr
// CHECK: load
// CHECK: ref_element_addr
// CHECK: struct_element_addr
// CHECK: load
// CHECK-NOT: bb1
// CHECK-NOT: ref_element_addr
// CHECK-NOT: struct_element_addr
// CHECK-NOT: load
// CHECK: return
@inline(never)
public func testUseLet(a:A0) -> Int32 {
  var counter: Int32
  // a.x and a.y should be loaded only once.
  counter = a.sum2() + a.sum2()
  counter += a.sum2() + a.sum2()
  return counter
}


@inline(never)
func getVal() -> Int32 {
   return 9
}


// Global let
let gx: Int32 = getVal()
let gy: Int32 = 1

func sum3() -> Int32 {
    // gx and gy should be loaded only once.
    let n = gx + gy
    action()
    let m = gx - gy
    action()
    let p = gx - gy + 1
    return n + m + p
}


// Check that gx and gy are loaded only once and then reused.
// CHECK-LABEL: sil {{.*}}testUseGlobalLet
// CHECK: bb0
// CHECK: global_addr @_Tv15let_propagation2gyVSs5Int32
// CHECK: global_addr @_Tv15let_propagation2gxVSs5Int32
// CHECK: struct_element_addr
// CHECK: load
// CHECK: struct_element_addr
// CHECK: load
// CHECK-NOT: bb1
// CHECK-NOT: global_addr
// CHECK-NOT: ref_element_addr
// CHECK-NOT: struct_element_addr
// CHECK-NOT: load
// CHECK: return
@inline(never)
public func testUseGlobalLet() -> Int32 {
  var counter: Int32 = 0
  // gx and gy should be loaded only once.
  counter = sum3() + sum3() + sum3() + sum3()
  return counter
}

struct A1 {
  let x: Int32
  
  // TODO: If VarDecl would have a field for initializers like this,
  // we could propagate the value of the initializer into all instructions
  // that use it, which in turn would allow for better constant
  // propagation.
  let y: Int32 = 100
  
  init(v: Int32) {
    if v > 0 {
      x = 1
    } else {
      x = -1
    }
  }
  // CHECK-LABEL: sil hidden @_TFV15let_propagation2A12f1fS0_FT_VSs5Int32
  // CHECK: bb0
  // CHECK: struct_extract {{.*}}#A1.x
  // CHECK: struct_extract {{.*}}#Int32.value
  // CHECK-NOT: load
  // CHECK-NOT: struct_extract
  // CHECK-NOT: struct_element_addr
  // CHECK-NOT: ref_element_addr
  // CHECK-NOT: bb1
  // CHECK: return
  func f1() -> Int32 {
    // x should be loaded only once.
    return x + x
  }

  // CHECK-LABEL: sil hidden @_TFV15let_propagation2A12f2fS0_FT_VSs5Int32
  // CHECK: bb0
  // CHECK: struct_extract {{.*}}#A1.y
  // CHECK: struct_extract {{.*}}#Int32.value
  // CHECK-NOT: load
  // CHECK-NOT: struct_extract
  // CHECK-NOT: struct_element_addr
  // CHECK-NOT: ref_element_addr
  // CHECK-NOT: bb1
  // CHECK: return
  func f2() -> Int32 {
    // load y only once.
    return y + y
  }

}


class A2 {
  let x: B2 = B2()
  // CHECK-LABEL: sil hidden @_TFC15let_propagation2A22affS0_FT_VSs5Int32
  // bb0
  // CHECK: %[[X:[0-9]+]] = ref_element_addr {{.*}}A2.x
  // CHECK-NEXT: load %[[X]]
  // CHECK: ref_element_addr {{.*}}B2.i
  // CHECK: %[[XI:[0-9]+]] = struct_element_addr {{.*}}#Int32.value
  // CHECK-NEXT: load %[[XI]]
  // return
  func af() -> Int32 {
    // x and x.i should be loaded only once.
    return x.f() + x.f()
  }
}

final class B2 {
  var i: Int32 = 10
  func f() -> Int32 {
     return i
  }
}
