// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15
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

struct TestView: View {
  @FakeState var angle: Angle = Angle(radians: Double.pi/1.5)
  let size = CGSize(width: 800, height: 600)
  var body: some View {
    Path { path in
      path.move(to: CGPoint(x: size.width/cos(angle.radians),
                            y: size.height/sin(angle.radians)))
      path.addLine(to: CGPoint(x: 100, y: 100))
    }.stroke(Color.red)
  }
}
