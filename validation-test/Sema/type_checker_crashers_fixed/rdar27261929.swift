// RUN: %target-swift-frontend %s -typecheck -verify

// Note: this used to type check successfully in Swift 3. In Swift 4, it produces
// an error, so probably this test isn't testing anything useful anymore, since
// we already exercise many such cases in test/Constraints/tuple_arguments.swift.
// However, there's no harm in keeping it around in case it exposes other bugs later.

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
      _ = R<([O], I)>.value(output, input) // expected-error {{enum case 'value' expects a single parameter of type '([O], I)'}}
      return R<([O], I)>.value((output, input))
    }
  }
}
