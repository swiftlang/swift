// RUN: %target-swift-frontend %s -typecheck -verify

public enum R<V> {
  case value(V)
}

public struct P<I, O> {
  public var run: (I) -> R<(O, I)>

  public init(run: @escaping (I) -> R<(O, I)>) {
    self.run = run
  }

  public func test() -> P<I, [O]> {
    return P<I, [O]> { input in
      var output: [O] = []
      _ = R<([O], I)>.value(output, input) // expected-error{{extra argument in call}}
      return R<([O], I)>.value((output, input))
    }
  }
}
