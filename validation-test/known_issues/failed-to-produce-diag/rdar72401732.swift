// rdar://72401732
// RUN: %target-typecheck-verify-swift

protocol View {}
protocol ShapeStyle {}

struct Color: View, ShapeStyle {
  static let white = Color()
}

extension ShapeStyle where Self == Color {
  static var white: Color { Color() }
}

extension View {
  func frame(height: Double? = nil) -> Self { self }
}

struct Size { var height: Double }
struct GeometryProxy { var size: Size }

func test(geo: GeometryProxy) {
  _ = Color.white.frame(height: geo.size.height*.2)
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
  // expected-error@-2 {{'*' is not a postfix unary operator}}
}
