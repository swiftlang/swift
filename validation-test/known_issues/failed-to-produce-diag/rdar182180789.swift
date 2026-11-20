// rdar://182180789
// RUN: %target-typecheck-verify-swift

protocol View {}

protocol ShapeStyle {}
struct Color: ShapeStyle {
  static var blue: Color { Color() }
}

protocol Shape: View {}
struct Circle: Shape {}

struct _ShapeView<Content: Shape, Style: ShapeStyle>: View {}

extension Shape {
  @_disfavoredOverload
  func fill<S: ShapeStyle>(_ content: S) -> some View { self }
  func fill<S: ShapeStyle>(_ content: S) -> _ShapeView<Self, S> { fatalError() }
}

protocol NSGestureRecognizerRepresentable {}

extension View {
  func gesture(_ representable: some NSGestureRecognizerRepresentable) -> some View { self }
}

struct Example: NSGestureRecognizerRepresentable {}

func test() -> some View {
  Circle()
    .fill(Color.blue)
    .gesture(Example().nonExistentMethod()) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
