// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

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
  @State var symbol: String = "timer"
  @State var selectedColor: ColorScheme = ColorScheme.pink

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
