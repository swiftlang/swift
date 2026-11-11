// rdar://69177839
// RUN: %target-typecheck-verify-swift

protocol View {
  associatedtype Body: View
  var body: Body { get }
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
  static func buildBlock<each C: View>(_ c: repeat each C) -> TupleView<(repeat each C)> { TupleView((repeat each c)) }
  static func buildIf<C: View>(_ c: C?) -> C? { c }
}
typealias CGFloat = Double
struct Edge {
  struct Set: OptionSet {
    let rawValue: Int
  }
}
struct Color: View {
  init(red: Double, green: Double, blue: Double) {}
  static let white = Color(red: 1, green: 1, blue: 1)
  var body: Never { fatalError() }
}
struct Button<Label: View>: View {
  init(action: @escaping () -> Void, @ViewBuilder label: () -> Label) {}
  var body: Never { fatalError() }
}
struct VStack<Content: View>: View {
  init(@ViewBuilder content: () -> Content) {}
  var body: Never { fatalError() }
}
struct HStack<Content: View>: View {
  init(@ViewBuilder content: () -> Content) {}
  var body: Never { fatalError() }
}
struct ZStack<Content: View>: View {
  init(@ViewBuilder content: () -> Content) {}
  var body: Never { fatalError() }
}
extension View {
  func padding(_ edges: Edge.Set = .all, _ length: CGFloat? = nil) -> some View { self }
  func padding(_ length: CGFloat) -> some View { self }
  func background<B: View>(_ b: B) -> some View { self }
  func overlay<O: View>(_ o: O) -> some View { self }
}
extension Edge.Set {
  static let all = Edge.Set(rawValue: 7)
}
struct Tile: View {
    var flag: Bool = false
    var mode: Bool = false
    var body: some View {
        VStack {
        }
        .overlay( // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
            ZStack {
                if self.flag {
                    HStack {
                        VStack {
                            if self.mode {
                                Button(action: {
                                }) {
                                }
                                .padding(15)
                                .background(self.backColor())
                            }
                        }
                    }
                }
            }
        )
    }
}
