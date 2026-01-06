// RUN: %target-swift-frontend -enable-implicit-dynamic -target %target-swift-5.1-abi-triple -parse-as-library -module-name=test -O -primary-file %s -emit-ir > %t.ll
// RUN: %FileCheck %s < %t.ll

protocol P { }

protocol Q {
  associatedtype AT: P
  func getAT() -> AT
}

private struct X: Q {
  private struct Inner: P { }

  // CHECK: @"$s4test1X33_58D62B15E5EAC1447430E52C74EAD489LLV5getATQryFQOMQ" = internal constant
  func getAT() -> some P {
    return Inner()
  }
}

func testMe<T: Q>(_ t: T) {
  _ = t.getAT()
}

func doIt() {
  testMe(X())
}
