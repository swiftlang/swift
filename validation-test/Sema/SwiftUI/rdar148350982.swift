// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15
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

final class Item: Identifiable, ObservableObject {
  var id = 42
  var text: String

  init(index: Int) {
    self.text = "Item \(index)"
  }
}

enum MessageKind: CaseIterable, Hashable, Identifiable {
  var id: Self { self }

  case channel
  case system
}

struct ContentView: View {
  @FakeState var items = (0..<100).map { Item(index: $0) }
  var body: some View {
    List {
      ForEach($items) { $item in
        HStack {
          Text(item.text)
          ForEach(MessageKind.allCases) { kind in
            Text("\(kind.noSuchMember)") // expected-error {{value of type 'MessageKind' has no member 'noSuchMember'}}
          }
        }
      }
    }
  }
}
