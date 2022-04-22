// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct MyView: View {
  var	data: [Transaction]

  var body: some View {
    Table(self.data) {
      // expected-error@-1 {{cannot infer return type of empty closure}} {{23-23=<#result#>}}
    }
  }
}
