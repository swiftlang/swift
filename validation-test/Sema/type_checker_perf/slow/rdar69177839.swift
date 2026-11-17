// rdar://69177839
// RUN: %target-typecheck-verify-swift -solver-scope-threshold=500

typealias CGFloat = Double

protocol View {
  associatedtype Body: View
  @ViewBuilder var body: Body { get }
}

extension Never: View {
  var body: Never { fatalError() }
}

struct TupleView<T>: View {
  init(_ value: T) {}
  var body: Never { fatalError() }
}

extension Optional: View where Wrapped: View {
  var body: Never { fatalError() }
}

@resultBuilder
struct ViewBuilder {
  static func buildBlock<Content>(_ content: Content) -> Content { content }
  static func buildBlock<each Content>(_ content: repeat each Content) -> TupleView<(repeat each Content)> where repeat each Content: View { TupleView((repeat each content)) }
  static func buildIf<Content>(_ content: Content?) -> Content? { content }
}

enum Edge {
  case trailing
  struct Set: OptionSet {
    let rawValue: Int
    static let top = Set(rawValue: 1)
    static let leading = Set(rawValue: 2)
    static let bottom = Set(rawValue: 4)
    static let trailing = Set(rawValue: 8)
    static let all = Set(rawValue: 15)
    static let horizontal = Set(rawValue: 10)
  }
}

struct EdgeInsets {}

struct HorizontalAlignment {
  static let center = HorizontalAlignment()
}

struct VerticalAlignment {
  static let center = VerticalAlignment()
}

struct Alignment {
  static let center = Alignment()
}

struct Font {
  static func custom(_ name: String, size: CGFloat) -> Font { Font() }
}

protocol ShapeStyle {}

struct Color {
  init(white: Double) {}
  static let white = Color(white: 1)
}

extension Color: ShapeStyle {}

extension ShapeStyle where Self == Color {
  static var white: Color { .white }
}

struct Animation {
  static let `default` = Animation()
}

protocol Transition {}

struct AnyTransition {
  static func move(edge: Edge) -> AnyTransition { AnyTransition() }
}

struct LocalizedStringResource: ExpressibleByStringInterpolation {
  init(stringLiteral value: String) {}
}

struct Text {
  init(_ resource: LocalizedStringResource) {}
  func font(_ font: Font?) -> Text { self }
}

extension Text: View {
  var body: Never { fatalError() }
}

enum ButtonRole {}

struct Button<Label: View>: View {
  init(action: @escaping () -> Void, @ViewBuilder label: () -> Label) {}
  var body: Never { fatalError() }
}

struct DefaultButtonLabel: View {
  var body: Never { fatalError() }
}

extension Button where Label == DefaultButtonLabel {
  init(role: ButtonRole, action: @escaping () -> Void) {}
}

struct VStack<Content: View>: View {
  init(alignment: HorizontalAlignment = .center, spacing: CGFloat? = nil, @ViewBuilder content: () -> Content) {}
  var body: Never { fatalError() }
}

struct HStack<Content: View>: View {
  init(alignment: VerticalAlignment = .center, spacing: CGFloat? = nil, @ViewBuilder content: () -> Content) {}
  var body: Never { fatalError() }
}

struct ZStack<Content: View>: View {
  init(alignment: Alignment = .center, @ViewBuilder content: () -> Content) {}
  var body: Never { fatalError() }
}

struct CoordinateSpace {
  static let local = CoordinateSpace()
}
protocol CoordinateSpaceProtocol {}
struct LocalCoordinateSpace: CoordinateSpaceProtocol {}
extension CoordinateSpaceProtocol where Self == LocalCoordinateSpace {
  static var local: LocalCoordinateSpace { LocalCoordinateSpace() }
}
struct CGPoint {}

extension View {
  func padding(_ insets: EdgeInsets) -> some View { self }
  func padding(_ edges: Edge.Set = .all, _ length: CGFloat? = nil) -> some View { self }
  func padding(_ length: CGFloat) -> some View { self }

  func background<Background: View>(_ background: Background, alignment: Alignment = .center) -> some View { self }
  func background<S: ShapeStyle>(_ style: S, ignoresSafeAreaEdges edges: Edge.Set = .all) -> some View { self }

  func onTapGesture(count: Int = 1, perform action: @escaping () -> Void) -> some View { self }
  func onTapGesture(count: Int = 1, coordinateSpace: CoordinateSpace = .local, perform action: @escaping (CGPoint) -> Void) -> some View { self }
  func onTapGesture(count: Int = 1, coordinateSpace: some CoordinateSpaceProtocol = .local, perform action: @escaping (CGPoint) -> Void) -> some View { self }

  func overlay<Overlay: View>(_ overlay: Overlay, alignment: Alignment = .center) -> some View { self }
  func overlay<S: ShapeStyle>(_ style: S, ignoresSafeAreaEdges edges: Edge.Set = .all) -> some View { self }

  func font(_ font: Font?) -> some View { self }
  func foregroundColor(_ color: Color?) -> some View { self }

  func transition(_ t: AnyTransition) -> some View { self }
  func transition<T: Transition>(_ transition: T) -> some View { self }

  func animation(_ animation: Animation?) -> some View { self }
}

extension View where Self: Equatable {
  func animation(_ animation: Animation?) -> some View { self }
}

struct Tile: View {
  var showA: Bool = false
  var showB: Bool = false
  var choice: Int = 0
  var sizeB: CGFloat = 18
  var pad: CGFloat = 15

  var body: some View {
    VStack {}
    .background(Color.white)
    .padding([.leading, .trailing, .bottom])
    .onTapGesture {}
    .overlay(
      ZStack {
        if self.showA {
          HStack {
            VStack {
              if !self.showB || self.choice == 0 {
                Button(action: {}) {
                  Text("0") // expected-error {{reasonable time}}
                    .font(.custom("X", size: self.sizeB))
                    .foregroundColor(self.textColor(at: 0))
                }
                .padding(self.pad)
                .padding(.top)
              }
            }
          }
          .padding(.horizontal)
          .padding(.horizontal)
          .transition(.move(edge: .trailing))
          .animation(.default)
        }
      }
    )
  }
}
