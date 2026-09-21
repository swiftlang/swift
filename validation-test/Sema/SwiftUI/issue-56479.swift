// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// https://github.com/apple/swift/issues/56479

import SwiftUI
import Foundation

// Stand-in for SwiftUI's `@State`, which became a macro in recent SDKs and would
// otherwise require the SwiftUIMacros plugin. This test exercises type-checking
// diagnostics, not `@State` itself; the box reproduces `@State`'s nonmutating
// setter and `Binding` projected value.
@propertyWrapper
struct FakeState<Value> {
  final class Box { var value: Value; init(_ value: Value) { self.value = value } }
  private let box: Box
  init(wrappedValue: Value) { box = Box(wrappedValue) }
  var wrappedValue: Value {
    get { box.value }
    nonmutating set { box.value = newValue }
  }
  var projectedValue: Binding<Value> {
    Binding(get: { box.value }, set: { box.value = $0 })
  }
}

struct ContentView: View {
  @FakeState private var date = Date()

  var body: some View {
    Group {
      DatePicker("Enter a date", selection: $date, displayedComponents: .date, in: Date())
      // expected-error@-1 {{argument 'in' must precede argument 'displayedComponents'}} {{78-90=}} {{52-52=in: Date(), }}
      DatePicker("Enter a date", selection: $date, displayedComponents: .date, in: Date() ... Date().addingTimeInterval(100))
    }
  }
}
