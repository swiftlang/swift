// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

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

struct Example: View {
  struct Item: Identifiable {
    var id: Int
    var title: String
    var isOn: Bool = .random()
  }

  @FakeState var items = [
    Item(id: 0, title: "")
  ]

  var body: some View {
    List {
      Section {
        ForEach($items) { $item in
          Toggle(item.title, isOn: $item.isOn.animation())
        }
        .onDelete { items.remove(atOffsets: $0) }
      }
    }
  }
}
