// rdar://167712363 (https://github.com/swiftlang/swift/issues/86330)
// RUN: %target-typecheck-verify-swift

protocol View {}
protocol ShapeStyle {}

struct Color: ShapeStyle {
  static var white: Color { Color() }
}
struct LinearGradient: ShapeStyle {}

struct Text: View {
  init(_ s: String) {}
  func bold() -> Text { self }
  func foregroundStyle<S: ShapeStyle>(_ s: S) -> Text { self }
}

extension View {
  func foregroundStyle<S: ShapeStyle>(_ s: S) -> some View { self }
}

func test(name: String, gradient: LinearGradient?) -> some View {
  Text(name)
    .bold()
    .foregroundStyle(gradient ?? Color.white) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

func test2(a: UInt8?, b: Int8) {
  _ = Int8(a ?? b)
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
