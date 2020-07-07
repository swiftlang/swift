// RUN: %target-swift-frontend -emit-sil %s -O | %FileCheck %s


@_optimize(none)
@inline(never)
func blackhole<T>(_ x:T) {
}

// CHECK-LABEL: sil [noinline] @$s26allocboxtostack_localapply9testapplySiyF :
// CHECK-NOT: alloc_box
// CHECK: [[STK:%.*]] = alloc_stack $Int, var, name "x"
// CHECK-LABEL: } // end sil function '$s26allocboxtostack_localapply9testapplySiyF'
@inline(never)
public func testapply() -> Int {
  var x = 0
  @inline(never)
  func bar() -> Int {
    blackhole(x)
    return x + 1
  }
  @inline(never)
  func bas() -> Int {
    return bar()
  }
  return bas()
}

// CHECK-LABEL: sil [noinline] @$s26allocboxtostack_localapply12testtryapplySiyKF :
// CHECK-NOT: alloc_box
// CHECK: [[STK:%.*]] = alloc_stack $Int, var, name "x"
// CHECK-LABEL: } // end sil function '$s26allocboxtostack_localapply12testtryapplySiyKF'
@inline(never)
public func testtryapply() throws -> Int {
  var x = 0
  @inline(never)
  func bar() throws -> Int {
    blackhole(x)
    return x + 1
  }
  @inline(never)
  func bas() throws -> Int {
    return try bar()
  }
  let res = try bas()
  return res
}

// CHECK-LABEL: sil [noinline] @$s26allocboxtostack_localapply16testpartialapplySiyF :
// CHECK-NOT: alloc_box
// CHECK: [[STK:%.*]] = alloc_stack $Int, var, name "x"
// CHECK-LABEL: } // end sil function '$s26allocboxtostack_localapply16testpartialapplySiyF'
@inline(never)
public func testpartialapply() -> Int {
  var x = 0
  @inline(never)
  func bar() -> Int {
    blackhole(x)
    return x + 1
  }
  @inline(never)
  func bas() -> Int {
    return bar()
  }
  return {bas()}()
}

// CHECK-LABEL: sil [noinline] @$s26allocboxtostack_localapply12testtwoboxesSiyF :
// CHECK-NOT: alloc_box
// CHECK: [[STK1:%.*]] = alloc_stack $Int, var, name "x"
// CHECK: [[STK2:%.*]] = alloc_stack $Int, var, name "y"
// CHECK-LABEL: } // end sil function '$s26allocboxtostack_localapply12testtwoboxesSiyF'
@inline(never)
public func testtwoboxes() -> Int {
  var x = 0
  var y = 0
  @inline(never)
  func bar() -> Int {
    blackhole(x)
    return x + y 
  }
  @inline(never)
  func bas() -> Int {
    return bar()
  }
  return bas()
}

// CHECK-LABEL: sil [noinline] @$s26allocboxtostack_localapply14testboxescapesyycyF :
// CHECK: alloc_box ${ var Int }, var, name "x"
// CHECK-LABEL: } // end sil function '$s26allocboxtostack_localapply14testboxescapesyycyF'
@inline(never)
public func testboxescapes() -> (() -> ()) {
  var x = 0
  @inline(never)
  func bar() -> (() -> ()) {
    return {x + 1}
  }
  @inline(never)
  func bas() -> (() -> ()) {
    return bar()
  }
  return bas()
}

// CHECK-LABEL: sil [noinline] @$s26allocboxtostack_localapply9testrecurSiyF :
// CHECK: alloc_box ${ var Int }, var, name "x"
// CHECK-LABEL: } // end sil function '$s26allocboxtostack_localapply9testrecurSiyF'
@inline(never)
public func testrecur() -> Int {
  var x = 0
  @inline(never)
  func bar() -> Int {
    return x + bas()
  }
  @inline(never)
  func bas() -> Int {
    return bar()
  }
  return bas() + bar()
}
