// RUN: %target-swift-frontend %s -O -sil-verify-all -emit-sil -o /dev/null


// Check that TempLValueOpt does not create invalid SIL which causes a verification error.

public struct S<T> {
  internal var a: T? = nil
  public var b: T? {a}
  public var c: T? = nil
  
  public mutating func set() {
    c = b
  }
}

public class Klass{}

public func test() -> S<Klass> {
  var s = S<Klass>()
  s.set()
  return s
}


