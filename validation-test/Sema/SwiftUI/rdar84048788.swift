// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct MyEditMode {
}

struct ContentView: View {
  var editMode: MyEditMode?

  var body: some View {
    EmptyView()
      .toolbar {
        ToolbarItem(placement: .navigationBarTrailing) {
          Button(editMode? ? "Done" : "Edit") { // expected-error {{optional type 'MyEditMode?' cannot be used as a boolean; test for '!= nil' instead}}
            print("Pressed")
          }
        }
      }
  }
}
