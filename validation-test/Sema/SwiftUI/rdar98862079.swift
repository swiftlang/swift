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

struct NewBounds {
  var minBinding: Binding<Double>!
  var maxBinding: Binding<Double>!
}

func test<V>(
  value: Binding<V>,
  in bounds: ClosedRange<V>
) -> some View where V : BinaryFloatingPoint, V.Stride : BinaryFloatingPoint {
  EmptyView()
}

struct MyView : View {
  @FakeState var newBounds: NewBounds
  var bounds: ClosedRange<Double>

  var body: some View {
    test(value: true ? $newBounds.maxBinding : $newBounds.minBinding, in: bounds)
    // expected-error@-1 2 {{result values in '? :' expression have mismatching types 'Binding<Binding<Double>?>' and 'Binding<Double>'}}
    // expected-note@-2 2 {{arguments to generic parameter 'Value' ('Binding<Double>?' and 'Double') are expected to be equal}}
  }
}
