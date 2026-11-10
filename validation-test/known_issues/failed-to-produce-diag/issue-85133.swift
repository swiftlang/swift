// rdar://163399766 (https://github.com/swiftlang/swift/issues/85133)
// RUN: %target-typecheck-verify-swift

struct Font {
  enum TextStyle { case body }

  static func system(_ style: TextStyle) -> Font { Font() }
  static func system(size: Double) -> Font { Font() }
}

struct Text {
  init(_ content: String) {}

  func font(_ font: Font?) -> Text { self }
}

func test(desc: String) {
  _ = Text(desc) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    .font(.system)
}

struct S {
  static func test(_: Int) -> S {}
  static func test(_: String) -> S {}
}

func test() -> S {
  .test // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

struct S2 {
  func foo() -> S2 { self }
  func foo(_: Bool) -> S2 { self }
}

func take(_: S2) {}

func test() {
  take(.foo) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

struct EdgeInsets {
  init() {}
  init(top: Double, leading: Double, bottom: Double, trailing: Double) {}
}

struct Image {
  func resizable(capInsets: EdgeInsets = EdgeInsets()) -> Image { self }
}

func test(image: Image) {
  _ = image.resizable(capInsets: .init) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

struct Font2 {
  func bold() -> Font2 { self }
  func bold(_ isActive: Bool) -> Font2 { self }
}

struct Text2 {
  func font(_ font: Font2?) -> Text2 { self }
}

func test(text: Text2) {
  _ = text.font(.bold) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
