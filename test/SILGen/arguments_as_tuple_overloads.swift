// RUN: %target-swift-frontend -parse-as-library -module-name=test -emit-silgen -enable-sil-ownership -primary-file %s | %FileCheck %s

// Check if we mangle the following constructors, functions, and
// subscripts correctly.

public struct Pair {
  // CHECK: sil @_T04test4PairVyACSi_SitcfC :
  public init(_ a: Int, _ b: Int) {
  }

  // CHECK: sil @_T04test4PairVyACSi_Sit_tcfC :
  public init(_ t: (Int, Int)) {
  }

  // CHECK: sil @_T04test4PairVAAyySi_SitF :
  public func test(_ a: Int, _ b: Int) {
  }

  // CHECK: sil @_T04test4PairVAAyySi_Sit_tF :
  public func test(_ t: (Int, Int)) {
  }

  // CHECK: sil @_T04test4PairVyS2i_Sitcig :
  public subscript(_:Int, _:Int) -> Int {
      get { return 0 }
  }

  // CHECK: sil @_T04test4PairVyS2i_Sit_tcig :
  public subscript(_:(Int, Int)) -> Int {
      get { return 0 }
  }
}

// CHECK: sil @_T04testAAyySi_SitF :
public func test(_ a: Int, _ b: Int) {
}

// CHECK: sil @_T04testAAyySi_Sit_tF :
public func test(_ t: (Int, Int)) {
}

// CHECK: sil @_T04test0A7NoLabelyySi_Sit_tF :
public func testNoLabel(_: (Int, Int)) {
}

// CHECK: sil @_T04test0A5FnArgyyySi_SitcF :
public func testFnArg(_: (Int, Int) -> Void) {
}

// CHECK: sil @_T04test0A5FnArgyyySi_Sit_tcF :
public func testFnArg(_: ((Int, Int)) -> Void) {
}

// CHECK: sil @_T04test3fooyyyt_tF :
public func foo(_: ()) {
}

// CHECK: sil @_T04test3fooyyF :
public func foo() {
}

public func baz() {
  // CHECK: function_ref @_T04test3bazyyFySi_Sit_tcfU_ :
  let _: ((Int, Int)) -> Void = { x in }

  // CHECK: function_ref @_T04test3bazyyFySi_SitcfU0_ :
  let _: (Int, Int) -> Void = { x, y in }
}
