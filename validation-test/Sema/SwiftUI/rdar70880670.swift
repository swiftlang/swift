// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

var foo = doesntExist // expected-error {{cannot find 'doesntExist' in scope}}

struct ContentView: View {
  var body: some View {
    VStack {
      Text(verbatim: foo)
    }
  }
}
