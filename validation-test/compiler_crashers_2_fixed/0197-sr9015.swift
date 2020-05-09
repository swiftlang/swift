// RUN: not %target-swift-frontend -typecheck -verify %s

private struct Collector {
  init() {}

  func appending(_ result: Int) -> Collector {
    return self
  }
}

func testing(ary: [String]) {
  let result1 = ary
    .compactMap { _ in
      result2.foo
    }
    .reduce(Collector()) { collector, result in collector.appending(result) }

  let result2 = result1 //.foobar
}
