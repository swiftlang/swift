// RUN: %target-swift-frontend -parse-as-library -module-name=test -emit-silgen -primary-file %s | %FileCheck %s

// Check if we mangle the following constructors and functions correctly.

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
}

// CHECK: sil @_T04testAAySi_SitF :
public func test(_ a: Int, _ b: Int) {
}

// CHECK: sil @_T04testAAySi_Sit_tF :
public func test(_ t: (Int, Int)) {
}

// CHECK: sil @_T04test3fooyt_tyF :
public func foo(_: ()) {
}

// CHECK: sil @_T04test3fooyyF :
public func foo() {
}
