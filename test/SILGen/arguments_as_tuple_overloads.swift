// RUN: %target-swift-frontend -parse-as-library -module-name=test -emit-silgen -primary-file %s | %FileCheck %s

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

  // CHECK: sil @_T04test4PairV9subscriptS2i_Sitcfg :
  public subscript(_:Int, _:Int) -> Int {
      get { return 0 }
  }

  // CHECK: sil @_T04test4PairV9subscriptS2i_Sit_tcfg :
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

// CHECK: sil @_T04test3fooyyt_tF :
public func foo(_: ()) {
}

// CHECK: sil @_T04test3fooyyF :
public func foo() {
}
