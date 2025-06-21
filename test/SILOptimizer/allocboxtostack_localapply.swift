// RUN: %target-swift-frontend -enable-copy-propagation=requested-passes-only -enable-lexical-lifetimes=false -emit-sil %s -O | %FileCheck %s


@_optimize(none)
@inline(never)
func blackhole<T>(_ x:T) {
}

// CHECK-LABEL: sil [noinline] @$s26allocboxtostack_localapply9testapplySiyF :
// CHECK-NOT: alloc_box
// CHECK: [[STK:%.*]] = alloc_stack [var_decl] $Int, var, name "x"
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
// CHECK: [[STK:%.*]] = alloc_stack [var_decl] $Int, var, name "x"
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
// CHECK: [[STK:%.*]] = alloc_stack [var_decl] $Int, var, name "x"
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
// CHECK: [[STK1:%.*]] = alloc_stack [var_decl] $Int, var, name "x"
// CHECK: [[STK2:%.*]] = alloc_stack [var_decl] $Int, var, name "y"
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
// CHECK:          alloc_stack [var_decl] $Int, var, name "x"
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

// Test to make sure AppliesToSpecialize in AllocBoxToStack is populated correctly when there are common function calls for multiple allox_boxes.
// Order of function calls constructed in PromotedOperands: bar common bas common.
// AppliesToSpecialize should have the order: bar bas common.
// Only then, the functions get specialized correctly, and we won't see an assert in checkNoPromotedBoxInApply.
// CHECK-LABEL: sil [noinline] @$s26allocboxtostack_localapply8testdfs1SiyF :
// CHECK-NOT: alloc_box ${ var Int }, var, name "x"
// CHECK-NOT: alloc_box ${ var Int }, var, name "y"
// CHECK-LABEL:} // end sil function '$s26allocboxtostack_localapply8testdfs1SiyF'
@inline(never)
public func testdfs1() -> Int {
  var x = 0
  var y = 0
  @inline(never)
  func bar() -> Int {
    return x
  }
  @inline(never)
  func bas() -> Int {
    return y
  }
  @inline(never)
  func common() -> Int {
    return bar() + bas()
  }
  return common()
}

// CHECK-LABEL: sil [noinline] @$s26allocboxtostack_localapply8testdfs2SiyF :
// CHECK:          alloc_stack [var_decl] $Int, var, name "x"
// CHECK:          alloc_stack [var_decl] $Int, var, name "y"
// CHECK-LABEL:} // end sil function '$s26allocboxtostack_localapply8testdfs2SiyF'
@inline(never)
public func testdfs2() -> Int {
  var x = 0
  var y = 0
  @inline(never)
  func bar() -> Int {
    return x
  }
  @inline(never)
  func bas() -> Int {
    return y
  }
  @inline(never)
  func innercommon() -> Int {
    return bar() + bas()
  }
  @inline(never)
  func local1() -> Int  {
    return innercommon()
  }
  @inline(never)
  func local2() -> Int  {
    return innercommon()
  }
  return local1() + local2()
}

// CHECK-LABEL: sil @$s26allocboxtostack_localapply15call2localfuncsSiyF :
// CHECK-NOT:     alloc_box
// CHECK-LABEL:} // end sil function '$s26allocboxtostack_localapply15call2localfuncsSiyF'
public func call2localfuncs() -> Int {
    var a1 = 1
    
    @inline(never)
    func innerFunction() {
        a1 += 1
    }

    innerFunction()
    innerFunction()

    return a1
}

