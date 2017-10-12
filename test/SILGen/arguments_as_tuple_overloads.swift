// RUN: %target-swift-frontend -parse-as-library -module-name=test -emit-silgen -enable-sil-ownership -primary-file %s | %FileCheck %s

// Check if we mangle the following constructors, functions, and
// subscripts correctly.

public struct Pair {
  // CHECK: sil @_T04test4PairVACSi_SitcfC :
  public init(_ a: Int, _ b: Int) {
  }

  // CHECK: sil @_T04test4PairVACSi_Sit_tcfC :
  public init(_ t: (Int, Int)) {
  }

  // CHECK: sil @_T04test4PairVAAySi_SitF :
  public func test(_ a: Int, _ b: Int) {
  }

  // CHECK: sil @_T04test4PairVAAySi_Sit_tF :
  public func test(_ t: (Int, Int)) {
  }

  // CHECK: sil @_T04test4PairVS2i_Sitcig :
  public subscript(_:Int, _:Int) -> Int {
      get { return 0 }
  }

  // CHECK: sil @_T04test4PairVS2i_Sit_tcig :
  public subscript(_:(Int, Int)) -> Int {
      get { return 0 }
  }
}

// CHECK: sil @_T04testAAySi_SitF :
public func test(_ a: Int, _ b: Int) {
}

// CHECK: sil @_T04testAAySi_Sit_tF :
public func test(_ t: (Int, Int)) {
}

// CHECK: sil @_T04test0A7NoLabelySi_Sit_tF :
public func testNoLabel(_: (Int, Int)) {
}

// CHECK: sil @_T04test0A5FnArgyySi_SitcF :
public func testFnArg(_: (Int, Int) -> Void) {
}

// CHECK: sil @_T04test0A5FnArgyySi_Sit_tcF :
public func testFnArg(_: ((Int, Int)) -> Void) {
}

// CHECK: sil @_T04test3fooyyt_tF :
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
