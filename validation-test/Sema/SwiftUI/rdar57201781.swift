// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -target %target-cpu-apple-macosx10.15 -swift-version 5

// REQUIRES: objc_interop
// REQUIRES: OS=macosx
// https://github.com/swiftlang/swift/issues/79255

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

struct ContentView : View {
  @FakeState var foo: [String] = Array(repeating: "", count: 5)

  var body: some View {
    VStack {
      HStack {
        Text("")
        TextFi // expected-error {{cannot find 'TextFi' in scope}}
      }

      ForEach(0 ... 4, id: \.self) { i in
        HStack {
          TextField("", text: self.$foo[i])
        }
      }
    }
  }
}
