// RUN: %target-typecheck-verify-swift -solver-enable-crash-on-valid-salvage

/// This file collects some random test cases where we used to find
/// a valid solution in salvage().

// Type variable was bound to an rvalue type too early
func merge(
  _ index: Int,
  _ values: inout Array<Int>,
  _ combine: (Int, Int) -> Int
) {
  { $0 = combine($0, index) }(&values[index])
}

// Over-eager binding inference for $T0 subtype () again
struct G<Element> {
  func remove(_: Element) -> Bool { return false }
  func remove(_: Int) -> Int { return 0 }

  func forEach(_: (Element) -> ()) {}
}

func f(x: G<String>, b: Bool) {
  x.forEach {
    if b {
      x.remove($0)  // expected-warning {{result of call to 'remove' is unused}}
    } else {
      g()
    }
  }
}

func g() {}
