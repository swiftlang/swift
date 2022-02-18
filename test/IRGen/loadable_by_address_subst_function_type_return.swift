// rdar://87792152
// RUN: %target-swift-frontend -disable-availability-checking -emit-ir -verify %s

public struct S1 {
  var a: Int?
  var b: Int?
  var c: Int?
}

public struct S2 {
  public func foo() {
    _ = bar
  }

  func bar(_: S1) -> some Any {
    return 0
  }
}
