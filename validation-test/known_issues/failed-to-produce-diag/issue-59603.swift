// rdar://170206936 (https://github.com/swiftlang/swift/issues/59603)
// RUN: %target-typecheck-verify-swift

protocol View {
  associatedtype Body: View
  @ViewBuilder var body: Body { get }
}

extension Never: View {
  var body: Never { fatalError() }
}

@resultBuilder
struct ViewBuilder {
  static func buildBlock<Content: View>(_ content: Content) -> Content { content }
}

protocol ShapeStyle {}

struct Color: View, ShapeStyle {
  var body: Never { fatalError() }
  static var red: Color { fatalError() }
  static var blue: Color { fatalError() }
  static var green: Color { fatalError() }
}

extension ShapeStyle where Self == Color {
  static var green: Color { fatalError() }
}

struct UnitPoint {
  static var top: UnitPoint { fatalError() }
  static var bottom: UnitPoint { fatalError() }
}

struct LinearGradient: View, ShapeStyle {
  init(colors: [Color], startPoint: UnitPoint, endPoint: UnitPoint) {}
  var body: Never { fatalError() }
}

struct Text: View {
  init(_ content: String) {}
  var body: Never { fatalError() }
}

struct Button<Label: View>: View {
  init(action: @escaping @MainActor () -> Void, @ViewBuilder label: () -> Label) {}
  var body: Never { fatalError() }
}

protocol ButtonStyle {}

extension View {
  func buttonStyle<S: ButtonStyle>(_ style: S) -> some View { self }
}

enum ButtonState {
  case normal
  case highlighted
}

class RoundedButtonStyleBuilder<Background: View & ShapeStyle> {
  var buttonProperties = [ButtonState: Background]()

  init() {}

  func color(_ color: Background, for buttonState: ButtonState) -> RoundedButtonStyleBuilder {
    self.buttonProperties[buttonState] = color
    return self
  }

  func build() -> RoundedButtonStyle<Background> {
    var roundedButtonStyle = RoundedButtonStyle<Background>()
    roundedButtonStyle.buttonProperties = buttonProperties
    return roundedButtonStyle
  }
}

struct RoundedButtonStyle<Background: View & ShapeStyle>: ButtonStyle {
  var buttonProperties = [ButtonState: Background]()
}

struct ReceiptView: View {
  var body: some View { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    Button(action: {}, label: { Text("Receipt") })
      .buttonStyle(
        RoundedButtonStyleBuilder()
          .color(LinearGradient(colors: [.red, .blue], startPoint: .top, endPoint: .bottom), for: .normal)
          .color(Color.green, for: .highlighted)
          .build()
      )
  }
}
