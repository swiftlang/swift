// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: concurrency

import _Concurrency

// SwiftUI-free stand-in to mirror onChange(of:_:)’s signature.
struct PseudoView {
  func onChange<T>(of value: T, _ action: @escaping (T, T) -> Void) -> PseudoView { self }
}

struct GenericView<S> {
  var s: S?
  var body: PseudoView {
    PseudoView()
      .onChange(of: s) { _, _ in
        // This used to be ambiguous between throwing/non-throwing Task.init overloads.
        _ = Task { } // expected-no-diagnostics
      }
  }
}
