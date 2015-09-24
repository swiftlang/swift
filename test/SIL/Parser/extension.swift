// RUN: %target-swift-frontend %s -emit-silgen | %target-sil-opt

public struct X {
}

extension X {
  public struct Y {}
}

extension X.Y {
  var startIndex: Int {
    return 0
  }
}

class X2<T> {
}

extension X2 {
  var startIndex: Int {
    return 0
  }
}

public func ==(lhs: X.Y, rhs: X.Y) -> Bool { return true }
