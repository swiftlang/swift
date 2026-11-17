// rdar://192707816 (https://github.com/swiftlang/swift/issues/68810)
// RUN: %target-typecheck-verify-swift

protocol View {
  associatedtype Body: View
  @ViewBuilder var body: Body { get }
}
extension Never: View { var body: Never { fatalError() } }

@resultBuilder struct ViewBuilder {
  static func buildBlock<C: View>(_ c: C) -> C { c }
  static func buildBlock<C0: View, C1: View>(_ c0: C0, _ c1: C1) -> Pair<C0, C1> { fatalError() }
}
struct Pair<A, B>: View { var body: Never { fatalError() } }
struct Text: View { init(_ s: String) {}; var body: Never { fatalError() } }
struct List<Content: View>: View { init(@ViewBuilder content: () -> Content) {}; var body: Never { fatalError() } }

protocol Rotor {}
@resultBuilder struct RotorBuilder {
  static func buildExpression<C: Rotor>(_ c: C) -> C { c }
  static func buildBlock<C: Rotor>(_ c: C) -> C { c }
  static func buildBlock<C0: Rotor, C1: Rotor>(_ c0: C0, _ c1: C1) -> C0 { c0 }
}

struct ForEach<Data: RandomAccessCollection, ID: Hashable, Content> {}
extension ForEach: View where Content: View {
  var body: Never { fatalError() }
  init(_ data: Data, id: KeyPath<Data.Element, ID>, @ViewBuilder content: @escaping (Data.Element) -> Content) {}
}
extension ForEach: Rotor where Content: Rotor {
  init(_ data: Data, id: KeyPath<Data.Element, ID>, @RotorBuilder content: @escaping (Data.Element) -> Content) {}
}

struct ContentView: View {
  var data: [String: Int]
  var body: some View { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    List {
      var index = 0
      ForEach(data.sorted(by: <), id: \.key) { key, value in
        Text(key)
        index += 1
      }
    }
  }
}
