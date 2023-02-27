// RUN: not %target-swift-frontend %s -typecheck

// rdar://105149979

protocol Component<Output> {
  associatedtype Output
}

struct R<Output> : Component {
}

extension RangeReplaceableCollection where Element: Equatable {
  func test<C: Collection, Replacement: Collection>(
    _ other: C,
    with replacement: Replacement,
    maxReplacements: Int = .max
  ) -> Self where C.Element == Element, Replacement.Element == Element {
    return self
  }
}

func test(str: inout String) {
  let elt = R<Substring>()

  _ = str.test {
    elt
    "+"
    elt
  } with: { match in
    ""
  }
}
