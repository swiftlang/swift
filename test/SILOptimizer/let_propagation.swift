// RUN: %target-swift-frontend -primary-file %s  -emit-sil -O | %FileCheck %s

// Check that LoadStoreOpts can handle "let" variables properly.
// Such variables should be loaded only once and their loaded values can be reused.
// This is safe, because once assigned, these variables cannot change their value.

// Helper function, which models an external functions with unknown side-effects.
// It is called just to trigger flushing of all known stored in LoadStore optimizations.
@inline(never)
func action() {
    print("")
}

final public class A0 {
 let x: Int32
 let y: Int32
 
 init(_ x: Int32, _ y: Int32) {
   self.x = x
   self.y = y
 }
 
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

/*
// DISABLE THIS TEST CASE FOR NOW. AS RLE GETS BETTER. WILL RE-ENABLE.
//
// Check that counter computation is completely evaluated
// at compile-time, because the value of a.x and a.y are known
// from the initializer and propagated into their uses, because
// we know that action() invocations do not affect their values.
//
// DISABLECHECK-LABEL: sil {{.*}}testAllocAndUseLet
// DISABLECHECK: bb0
// DISABLECHECK-NOT: ref_element_addr
// DISABLECHECK-NOT: struct_element_addr
// DISABLECHECK-NOT: bb1
// DISABLECHECK: function_ref @_T015let_propagation6actionyyF
// DISABLECHECK: apply
// DISABLECHECK: apply
// DISABLECHECK: apply
// DISABLECHECK: apply
// DISABLECHECK: apply
// DISABLECHECK: apply
// DISABLECHECK: apply
// DISABLECHECK: apply
// DISABLECHECK: integer_literal $Builtin.Int32, 36
// DISABLECHECK-NEXT: struct $Int32 ({{.*}} : $Builtin.Int32)
// DISABLECHECK-NEXT: return
@inline(never)
public func testAllocAndUseLet() -> Int32 {
  let a = A0(3, 1)
  var counter: Int32
  // a.x and a.y should be loaded only once.
  counter = a.sum2() + a.sum2()
  counter += a.sum2() + a.sum2()
  return counter
}
*/


/*
// DISABLE THIS TEST CASE FOR NOW. AS RLE GETS BETTER. WILL RE-ENABLE.
//
// Check that a.x and a.y are loaded only once and then reused.
// DISABLECHECK-LABEL: sil {{.*}}testUseLet
// DISABLECHECK: bb0
// DISABLECHECK: ref_element_addr
// DISABLECHECK: struct_element_addr
// DISABLECHECK: load
// DISABLECHECK: ref_element_addr
// DISABLECHECK: struct_element_addr
// DISABLECHECK: load
// DISABLECHECK-NOT: bb1
// DISABLECHECK-NOT: ref_element_addr
// DISABLECHECK-NOT: struct_element_addr
// DISABLECHECK-NOT: load
// DISABLECHECK: return
@inline(never)
public func testUseLet(a: A0) -> Int32 {
  var counter: Int32
  // a.x and a.y should be loaded only once.
  counter = a.sum2() + a.sum2()
  counter += a.sum2() + a.sum2()
  return counter
}
*/


struct Goo {
  var x: Int32
  var y: Int32
}

struct Foo { 
  var g: Goo
}

struct Bar { 
  let f: Foo
  var h: Foo
  
  @inline(never)
  mutating func action() {
  }
}

@inline(never)
func getVal() -> Int32 {
   return 9
}


// Global let
let gx: Int32 = getVal()
let gy: Int32 = getVal()

func sum3() -> Int32 {
    // gx and gy should be loaded only once.
    let n = gx + gy
    action()
    let m = gx - gy
    action()
    let p = gx - gy + 1
    return n + m + p
}


/*
// DISABLE THIS TEST CASE FOR NOW. AS RLE GETS BETTER. WILL RE-ENABLE.
//
// Check that gx and gy are loaded only once and then reused.
// DISABLECHECK-LABEL: sil {{.*}}testUseGlobalLet
// DISABLECHECK: bb0
// DISABLECHECK: global_addr @_T015let_propagation2gys5Int32Vv
// DISABLECHECK: global_addr @_T015let_propagation2gxs5Int32Vv
// DISABLECHECK: struct_element_addr
// DISABLECHECK: load
// DISABLECHECK: struct_element_addr
// DISABLECHECK: load
// DISABLECHECK-NOT: bb1
// DISABLECHECK-NOT: global_addr
// DISABLECHECK-NOT: ref_element_addr
// DISABLECHECK-NOT: struct_element_addr
// DISABLECHECK-NOT: load
// DISABLECHECK: return
@inline(never)
public func testUseGlobalLet() -> Int32 {
  var counter: Int32 = 0
  // gx and gy should be loaded only once.
  counter = sum3() + sum3() + sum3() + sum3()
  return counter
}
*/

struct A1 {
  private let x: Int32
  
  // Propagate the value of the initializer into all instructions
  // that use it, which in turn would allow for better constant
  // propagation.
  private let y: Int32 = 100
  
  init(v: Int32) {
    if v > 0 {
      x = 1
    } else {
      x = -1
    }
  }

  // CHECK-LABEL: sil hidden @_T015let_propagation2A1V2f1{{[_0-9a-zA-Z]*}}F
  // CHECK: bb0
  // CHECK: struct_extract {{.*}}#A1.x
  // CHECK: struct_extract {{.*}}#Int32._value
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

  // CHECK-LABEL: sil hidden @_T015let_propagation2A1V2f2{{[_0-9a-zA-Z]*}}F
  // CHECK: bb0
  // CHECK: integer_literal $Builtin.Int32, 200
  // CHECK-NEXT: struct $Int32
  // CHECK-NEXT: return
  func f2() -> Int32 {
    // load y only once.
    return y + y
  }

}


class A2 {
  let x: B2 = B2()
  // CHECK-LABEL: sil hidden @_T015let_propagation2A2C2af{{[_0-9a-zA-Z]*}}F
  // bb0
  // CHECK: %[[X:[0-9]+]] = ref_element_addr {{.*}}A2.x
  // CHECK-NEXT: load %[[X]]
  // CHECK: ref_element_addr {{.*}}B2.i
  // CHECK: %[[XI:[0-9]+]] = struct_element_addr {{.*}}#Int32._value
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

@inline(never)
func oops() {

}

struct S {
  let elt : Int32
}

// Check that we can handle reassignments to a variable
// of struct type properly.
// CHECK-LABEL: sil {{.*}}testStructWithLetElement
// CHECK-NOT: function_ref @{{.*}}oops
// CHECK: return 
public func testStructWithLetElement() -> Int32 {
    var someVar = S(elt: 12)
    let tmp1 = someVar.elt

    someVar = S(elt: 15)
    let tmp2 = someVar.elt

    // This check should get eliminated
    if (tmp2 == tmp1) {
       // If we get here, the compiler has propagated
       // the old value someVar.elt into tmp2, which 
       // is wrong.
       oops()
    }
    return tmp1+tmp2
}


public typealias Tuple3 = (Int32, Int32, Int32)

final public class S3 {
  let x: Tuple3
  var y: Tuple3
  
  init(x: Tuple3, y:Tuple3) {
    self.x = x
    self.y = y
  }
}


/*
// DISABLE THIS TEST CASE FOR NOW. AS RLE GETS BETTER. WILL RE-ENABLE.
//
// Check that s.x.0 is loaded only once and then reused.
// DISABLECHECK-LABEL: sil {{.*}}testLetTuple
// DISABLECHECK: tuple_element_addr
// DISABLECHECK: %[[X:[0-9]+]] = struct_element_addr
// DISABLECHECK: load %[[X]]
// DISABLECHECK-NOT: load %[[X]]
// DISABLECHECK: return
public func testLetTuple(s: S3) -> Int32 {
  var counter: Int32 = 0
  counter += s.x.0
  action()
  counter += s.x.0
  action()
  counter += s.x.0
  action()
  counter += s.x.0
  action()
  return counter
}
*/

// Check that s.x.0 is reloaded every time.
// CHECK-LABEL: sil {{.*}}testVarTuple
// CHECK: tuple_element_addr
// CHECK: %[[X:[0-9]+]] = struct_element_addr
// CHECK: load %[[X]]
// CHECK: load %[[X]]
// CHECK: load %[[X]]
// CHECK: load %[[X]]
// CHECK: return
public func testVarTuple(s: S3) -> Int32 {
  var counter: Int32 = 0
  counter += s.y.0
  action()
  counter += s.y.0
  action()
  counter += s.y.0
  action()
  counter += s.y.0
  action()
  return counter
}


