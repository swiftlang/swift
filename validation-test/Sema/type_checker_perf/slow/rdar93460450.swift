// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx12 -swift-version 5 -solver-scope-threshold=10000
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI
import Foundation

struct Value: Identifiable {
  var id: UUID
  var name: String
}

struct MySettingsView: View {
  init() {}
  var body: some View { EmptyView() }
}

struct MyView: View {
  var data: [Value]
  @State var selection: String?

  var body: some View {
    List(selection: Binding(get: {
      selection ?? data.first?.name
    }, set: { newValue in
    })) {
      ForEach(data, id: \.id) { v in
        NavigationLink(value: v.name) {
          HStack {
            MySettingsView(x: 42, y: v.name)
            // expected-error@-1 {{reasonable time}}
          }
        }
      }
    }
  }
}
