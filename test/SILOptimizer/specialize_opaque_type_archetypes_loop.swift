// RUN: %target-swift-frontend -emit-sil -O %s -disable-availability-checking

// This should not hang forever.

public func foo() -> some P {
  return S()
}

public protocol P {
  associatedtype A: P
  func p() -> A
}

public struct S: P {
  public func p() -> some P { return self }
}

public func bar<T: P>(_ t: T, _ n: Int) {
  if n > 0 { bar(t.p(), n - 1) }
}

bar(foo(), 1000)
