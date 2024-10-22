// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

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
  @State var newBounds: NewBounds
  var bounds: ClosedRange<Double>

  var body: some View {
    test(value: true ? $newBounds.maxBinding : $newBounds.minBinding, in: bounds)
    // expected-error@-1 2 {{result values in '? :' expression have mismatching types 'Binding<Binding<Double>?>' and 'Binding<Double>'}}
    // expected-note@-2 2 {{arguments to generic parameter 'Value' ('Binding<Double>?' and 'Double') are expected to be equal}}
  }
}
