// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5

// REQUIRES: concurrency
// REQUIRES: OS=macosx

import SwiftUI

@MainActor func someMainActorFn() {}

struct ContentView: View {
  var body: some View {
    VStack {
      Button(action: someMainActorFn) {
        Text("Sign In")
      }
    }
  }
}

struct ContentViewExplicitAnnotation: View {
  var body: some View {
    VStack { @MainActor in
      Button(action: someMainActorFn) {
        Text("Sign In")
      }
    }
  }
}
