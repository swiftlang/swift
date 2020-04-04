// RUN: %target-swift-frontend %s -O -emit-sil | %FileCheck %s

class Foo {
  var x : Int = 0
}

 func a( _ f : Foo) {
  f.x += 1
}

 func b( _ f : Foo) -> Int {
  return f.x + 2
}

// CHECK-LABEL: sil_global private @$s15stack_promotion8arr_testSiyFTv_
// CHECK-NOT: alloc_ref
// CHECK-NOT: ref_element_addr
// CHECK: %initval = object

// CHECK-LABEL: sil @$s15stack_promotion14simple_foo_useySiSbF
// CHECK-NOT: alloc_ref
// CHECK-NOT: ref_element_addr
// CHECK: [[I:%.*]] = integer_literal
// CHECK-NEXT: [[X:%.*]] = struct $Int ([[I]]
// CHECK-NEXT: [[SA:%.*]] = alloc_stack $Int
// CHECK-NEXT: store [[X]] to [[SA]] : $*Int
// CHECK-NEXT: [[BA:%.*]] = begin_access [read] [dynamic] [no_nested_conflict] [[SA]]
// CHECK-NEXT: [[VAL:%.*]] = load [[BA]]
// CHECK-NEXT: end_access [[BA]]
// CHECK-NEXT: dealloc_stack [[SA]]
// CHECK-NEXT: return [[VAL]]
// CHECK-LABEL: end sil function '$s15stack_promotion14simple_foo_useySiSbF'
public func simple_foo_use(_ check : Bool) -> Int {
  let f = Foo()
  return f.x
}

// CHECK-LABEL: sil @$s15stack_promotion3a_bySiSbF
// CHECK-NOT: alloc_ref
// CHECK-NOT: ref_element_addr
// CHECK: [[I:%.*]] = integer_literal
// CHECK-NEXT: [[X:%.*]] = struct $Int ([[I]]
// CHECK-NEXT: [[SA:%.*]] = alloc_stack $Int
// CHECK-NEXT: store [[X]] to [[SA]] : $*Int
// CHECK-LABEL: end sil function '$s15stack_promotion3a_bySiSbF'
public func a_b(_ check : Bool) -> Int {
  let f = Foo()
  if check {
    a(f)
  }
  return b(f)
}

class Bar {
  var x : [Int]
  init (_ x: [Int]) {
      self.x = x
  }
}

// CHECK-LABEL: sil @$s15stack_promotion8arr_testSiyF
// CHECK-NOT: alloc_ref
// CHECK-NOT: ref_element_addr
// CHECK: global_value @$s15stack_promotion8arr_testSiyFTv_ : $_ContiguousArrayStorage<Int>
// CHECK-LABEL: end sil function '$s15stack_promotion8arr_testSiyF'
public func arr_test() -> Int {
    let arr = [0, 1, 2, 3, 4]
    let f = Bar(arr)
    return f.x.count
}

class Gen<T> {
  var x : T
  init (_ x: T) {
      self.x = x
  }
}

// CHECK-LABEL: sil @$s15stack_promotion12test_genericyyF
// CHECK: bb0
// CHECK-NEXT: tuple
// CHECK-NEXT: return
// CHECK-LABEL: end sil function '$s15stack_promotion12test_genericyyF'
public func test_generic() {
  _ = Gen(0)
}
