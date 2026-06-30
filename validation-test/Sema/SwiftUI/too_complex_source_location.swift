// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// https://forums.swift.org/t/roadmap-for-improving-the-type-checker/82952/9
//
// The purpose of the test is to ensure the diagnostic points at the right statement in
// the function body, and not the function declaration itself.
//
// Ideally, we would produce a useful diagnostic here. Once we are able to do that, we
// will need to devise a new test which complains with 'reasonable time' to ensure the
// source location remains correct.

import SwiftUI

// A trivial stand-in for `@State` that avoids depending on the SwiftUI macro
// plugin. This test only exercises a type-checking pattern, not `@State`
// itself; all it needs is a `Binding` projected value for the `Picker`.
@propertyWrapper
struct FakeState<T> {
  var wrappedValue: T
  var projectedValue: Binding<T> { .constant(wrappedValue) }
}

struct ContentView: View {
    @FakeState var selection = ""

    @FakeState var a: Int?
    @FakeState var b: Int?
    @FakeState var c: Int?

    var body: some View {
        ScrollView {
          VStack {
            VStack {
              Picker(selection: $selection) {
                ForEach(["a", "b", "c"], id: \.self) {
                  Text($0)  // expected-error {{ reasonable time}}
                    .foregroundStyl(.red) // Typo is here
                }
              } label: {
              }
              .pickerStyle(.segmented)
            }
            .padding(.vertical)
          }
          .padding(.horizontal)
        }
        .onChange(of: a) { oldValue, newValue in
        }
        .onChange(of: b) { oldValue, newValue in
        }
        .onChange(of: c) { oldValue, newValue in
        }
    }
}
