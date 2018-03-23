// RUN: %target-typecheck-verify-swift

struct RequiresComparable<T: Comparable> { }

extension CountableRange { // expected-warning{{'CountableRange' is deprecated: renamed to 'Range'}}
  // expected-note@-1{{use 'Range' instead}}{{11-25=Range}}
  func testComparable() {
    _ = RequiresComparable<Bound>()
  }
}

