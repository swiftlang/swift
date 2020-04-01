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

// CHECK-LABEL: sil @$s6object14simple_foo_useySiSbF
// CHECK-NOT: alloc_ref
// CHECK: [[I:%.*]] = integer_literal
// CHECK-NEXT: [[X:%.*]] = struct $Int ([[I]]
// CHECK-NEXT: [[OBJ:%.*]] = object $Foo ([[X]] : $Int)
// CHECK-NEXT: [[E:%.*]] = ref_element_addr [[OBJ]] : $Foo, #Foo.x
// CHECK-NEXT: begin_access
// CHECK-NEXT: [[VAL:%.*]] = load
// CHECK-NEXT: end_access
// CHECK-NEXT: return [[VAL]]
// CHECK-LABEL: end sil function '$s6object14simple_foo_useySiSbF'
public func simple_foo_use(_ check : Bool) -> Int {
  let f = Foo()
  return f.x
}

// CHECK-LABEL: sil @$s6object3a_bySiSbF
// CHECK-NOT: alloc_ref
// CHECK: [[I:%.*]] = integer_literal
// CHECK-NEXT: [[X:%.*]] = struct $Int ([[I]]
// CHECK-NEXT: object $Foo (%3 : $Int)
// CHECK-LABEL: end sil function '$s6object3a_bySiSbF'
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

// CHECK-LABEL: sil @$s6object8arr_testSiyF
// CHECK-NOT: alloc_ref [stack]
// CHECK: object $Bar (%{{.*}} : $Array<Int>)
// CHECK-LABEL: end sil function '$s6object8arr_testSiyF'
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

// CHECK-LABEL: sil @$s6object12test_genericyyF
// CHECK-NOT: object
// CHECK: alloc_ref [stack] $Gen<Int>
// CHECK-LABEL: end sil function '$s6object12test_genericyyF'
public func test_generic() {
    _ = Gen(0)
}

class HasDeinit {
    var x : Int = 0
    deinit { }
}

// CHECK-LABEL: sil @$s6object11test_deinityyF
// CHECK-NOT: object
// CHECK: alloc_ref [stack] $HasDeinit
// CHECK-LABEL: end sil function '$s6object11test_deinityyF'
public func test_deinit() {
    _ = HasDeinit()
}
