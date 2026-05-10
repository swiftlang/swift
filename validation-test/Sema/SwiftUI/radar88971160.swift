// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

@MainActor
class ContentState: ObservableObject { }

@available(SwiftStdlib 5.3, *)
struct SomeView: View {
  @StateObject private var contentState = ContentState()

  var body: some View {
    Text("Hello, world!")
  }
}

