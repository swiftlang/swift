// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -target %target-cpu-apple-macosx10.15 -swift-version 5
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

enum ColorScheme: CaseIterable, Hashable, Equatable, Identifiable {
// expected-error@-1 {{type 'ColorScheme' does not conform to protocol 'Identifiable'}}
// expected-note@-2 {{add stubs for conformance}}
  case `default`
  case pink

  var foreground: Color {
    switch self {
    case .default:
      return .primary
    case .pink:
      return .pink
    }
  }
}

struct IconPicker : View {
  var body: some View {
    Text("hello")
  }
}

struct CountdownEditor : View {
  @FakeState var symbol: String = "timer"
  @FakeState var selectedColor: ColorScheme = ColorScheme.pink

  var body: some View {
    NavigationLink(destination: IconPicker()) {
      Text("Icon")
      Spacer()
      Image(systemName: symbol)
        .foregroundColor(selectedColor.color)
        // expected-error@-1 {{cannot convert value of type 'Binding<Subject>' to expected argument type 'Color'}}
        // expected-error@-2 {{referencing subscript 'subscript(dynamicMember:)' requires wrapper 'Binding<ColorScheme>'}}
        // expected-error@-3 {{value of type 'ColorScheme' has no dynamic member 'color' using key path from root type 'ColorScheme'}}
    }
  }
}
