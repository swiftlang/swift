// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx12 -swift-version 5 -solver-scope-threshold=10000
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI
import Foundation

// Stand-in for SwiftUI's `@State`, which became a macro in recent SDKs and would
// otherwise require the SwiftUIMacros plugin. This test exercises type-checking
// performance, not `@State` itself; the box reproduces `@State`'s nonmutating
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
  @FakeState var selection: String?

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
