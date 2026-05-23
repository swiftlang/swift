// RUN: %target-typecheck-verify-swift

// REQUIRES: OS=macosx

// Tests for the availability check in DesugarForEachStmt.

struct S {}

@available(macOS 27.0, *)
extension S: Sequence {
  struct Iterator: IteratorProtocol {
    func next() -> Int? { nil }
  }
  func makeIterator() -> Iterator {}
}

func testUnavailableSequenceConformance(_ xs: S) {
  for _ in xs {} // expected-error {{for-in loop requires 'S' to conform to 'Sequence', which is only available in macOS 27.0 or newer}}
  // expected-note@-2 {{add '@available' attribute to enclosing global function}}
  // expected-note@-2 {{add 'if #available' version check}} {{3-17=if #available(macOS 27.0, *) {\n      for _ in xs {\}\n  \} else {\n      // Fallback on earlier versions\n  \}}}
}

@available(SwiftStdlib 6.4, *)
func testAvailableSequenceConformance(_ xs: S) {
  for _ in xs {}
}

func testAvailableGuardSequenceConformance(_ xs: S) {
  if #available(SwiftStdlib 6.4, *) {
    for _ in xs {}
  }
}
