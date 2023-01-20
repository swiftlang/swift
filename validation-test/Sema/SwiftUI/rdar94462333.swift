// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5

// REQUIRES: concurrency
// REQUIRES: OS=macosx

import SwiftUI

@MainActor func someMainActorFn() {}

struct ContentView: View {
  var body: some View {
    VStack {
      // In the future this warning should go away too, but it remains since the isolation of the closure
      // passed to the VStack is not inferred yet during constraint solving.
      // expected-warning@+1 {{converting function value of type '@MainActor () -> ()' to '() -> Void' loses global actor 'MainActor'}}
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