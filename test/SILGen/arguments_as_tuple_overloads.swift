// RUN: %target-swift-emit-silgen -parse-as-library -module-name=test -enable-sil-ownership -primary-file %s | %FileCheck %s

// Check if we mangle the following constructors, functions, and
// subscripts correctly.

public struct Pair {
  // CHECK: sil @$S4test4PairVyACSi_SitcfC :
  public init(_ a: Int, _ b: Int) {
  }

  // CHECK: sil @$S4test4PairVyACSi_Sit_tcfC :
  public init(_ t: (Int, Int)) {
  }

  // CHECK: sil @$S4test4PairVAAyySi_SitF :
  public func test(_ a: Int, _ b: Int) {
  }

  // CHECK: sil @$S4test4PairVAAyySi_Sit_tF :
  public func test(_ t: (Int, Int)) {
  }

  // CHECK: sil @$S4test4PairVyS2i_Sitcig :
  public subscript(_:Int, _:Int) -> Int {
      get { return 0 }
  }

  // CHECK: sil @$S4test4PairVyS2i_Sit_tcig :
  public subscript(_:(Int, Int)) -> Int {
      get { return 0 }
  }
}

// CHECK: sil @$S4testAAyySi_SitF :
public func test(_ a: Int, _ b: Int) {
}

// CHECK: sil @$S4testAAyySi_Sit_tF :
public func test(_ t: (Int, Int)) {
}

// CHECK: sil @$S4test0A7NoLabelyySi_Sit_tF :
public func testNoLabel(_: (Int, Int)) {
}

// CHECK: sil @$S4test0A5FnArgyyySi_SitXEF :
public func testFnArg(_: (Int, Int) -> Void) {
}

// CHECK: sil @$S4test0A5FnArgyyySi_Sit_tXEF :
public func testFnArg(_: ((Int, Int)) -> Void) {
}

// CHECK: sil @$S4test3fooyyyt_tF :
public func foo(_: ()) {
}

// CHECK: sil @$S4test3fooyyF :
public func foo() {
}

public func baz() {
  // CHECK: function_ref @$S4test3bazyyFySi_Sit_tcfU_ :
  let _: ((Int, Int)) -> Void = { x in }

  // CHECK: function_ref @$S4test3bazyyFySi_SitcfU0_ :
  let _: (Int, Int) -> Void = { x, y in }
}
