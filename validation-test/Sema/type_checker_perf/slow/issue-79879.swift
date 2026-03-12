// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50
// REQUIRES: OS=macosx

// FIXME: We have a bug with our limits. We clear the counters
// on each conjunction step, but we might be inside of a
// disjunction scope, so we don't keep track of previous work
// done already. That is why the scope limit here is 50, it
// passes otherwise.

// https://github.com/swiftlang/swift/issues/79879

import SwiftUI

func foo() -> some View {
  Group {
    Group {
      Group {
        Text("") // expected-error {{reasonable time}}
        + Text("")
          .foregroundColor(Color(""))
        + Text("")
        + Text("")
        + Text("")
      }
    }
  }
}
