// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx12 -swift-version 5

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

@available(macOS 12, *)
struct S {}

@available(iOS, unavailable)
extension S {
  class NestedOtherPlatformUnavailable {
    @Published var x: Int = 0
    @Binding var y: Bool

    init(y: Binding<Bool>) {
      _y = y
    }
  }
}

@available(*, unavailable)
extension S {
  class NestedAlwaysUnavailable {
    @Published var x: Int = 0
    @Binding var y: Bool

    init(y: Binding<Bool>) {
      _y = y
    }
  }
}
