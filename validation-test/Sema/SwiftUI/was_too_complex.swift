// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// https://forums.swift.org/t/roadmap-for-improving-the-type-checker/82952/9
//
// This test formerly ran out of time. Now we catch the typo error
//

import SwiftUI

struct ContentView: View {
    @State var selection = ""

    @State var a: Int?
    @State var b: Int?
    @State var c: Int?

    var body: some View {
      ScrollView {
        Picker(selection: $selection) {
          ForEach(["a", "b", "c"], id: \.self) {
            Text($0)  // Formerly ran out of time
              .foregroundStyl(.red) // expected-error {{value of type 'Text' has no member 'foregroundStyl'; did you mean 'foregroundStyle'}}
            // expected-error@-1 {{cannot infer contextual base in reference to member 'red'}}
          }
        } label: {
        }
        .pickerStyle(.segmented)
      }
      .onChange(of: a) { oldValue, newValue in
      }
      .onChange(of: b) { oldValue, newValue in
      }
      .onChange(of: c) { oldValue, newValue in
      }
    }
}
