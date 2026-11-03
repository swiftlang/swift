// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// https://github.com/swiftlang/swift/issues/88533

import SwiftUI

struct ReproMinimalView: View {
  var body: some View {
    Text("body").toolbar { // expected-error {{ambiguous use of 'toolbar'}}
      Text("bare view in toolbar")
      ToolbarItem(placement: .principal) { Text("x") }
    }
  }
}
