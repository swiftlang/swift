// rdar://160990307
// RUN: %target-typecheck-verify-swift

protocol View {
  associatedtype Body: View
  @ViewBuilder var body: Body { get }
}

extension Never: View {
  typealias Body = Never
  var body: Never { fatalError() }
}

@resultBuilder
struct ViewBuilder {
  static func buildBlock<C: View>(_ c: C) -> C { c }
}

struct Anchor<Value> {}

struct Proxy {
  subscript<T>(_ anchor: Anchor<T>) -> T { fatalError() }
}

struct Reader<Content: View>: View {
  init(@ViewBuilder content: @escaping (Proxy) -> Content) {}
  var body: Never { fatalError() }
}

struct ForEach<Data: RandomAccessCollection, Content: View>: View {
  init(_ data: Data, @ViewBuilder content: @escaping (Data.Element) -> Content) {}
  var body: Never { fatalError() }
}

struct Shape: View {
  var body: Never { fatalError() }
}

struct Item {
  var anchor: Anchor<Int>
}

struct TestView: View {
  var items: [Item]

  var body: some View { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    Reader { proxy in
      ForEach(items) { item in
        let bounds = proxy[item]
        Shape()
      }
    }
  }
}
