// RUN: %target-swift-frontend -emit-sil %s

public class C {
  public func foo() -> Self {
    let arr = [self]

    bar(arr)

    return self
  }
}

@_transparent public func bar<T : Sequence>(_ xs: T) {
  for x in xs { _ = x }
}
