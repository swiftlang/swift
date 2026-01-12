// rdar://136754315
// RUN: %target-typecheck-verify-swift

protocol View {
  associatedtype Body: View
  @ViewBuilder var body: Body { get }
}

extension Never: View {
  var body: Never { fatalError() }
}

@resultBuilder struct ViewBuilder {
  static func buildBlock<C: View>(_ c: C) -> C { c }
}

struct Text: View {
  init(_ s: String) {}
  var body: Never { fatalError() }
}

struct ModifiedView<Content>: View {
  var body: Never { fatalError() }
}

struct ScrollView<Content: View>: View {
  init(_ axes: Int = 0, showsIndicators: Bool = true, @ViewBuilder content: () -> Content) {}
  init(_ axes: Int = 0, @ViewBuilder content: () -> Content) {}
  var body: Never { fatalError() }
}

extension View {
  func sheet<Content: View>(isPresented: Bool, @ViewBuilder content: @escaping () -> Content) -> ModifiedView<Self> { fatalError() }
}

struct ChildView: View {
  let items: [String]
  var body: some View { Text("") }
}

struct ParentView: View {
  var items: [Int]?

  var body: some View {
    ScrollView { Text("") }
      .sheet(isPresented: true) {
        // expected-note@+2{{arguments to generic parameter 'Element' ('Int' and 'String') are expected to be equal}}
        // expected-error@+1{{cannot convert value of type '[Int]' to expected argument type '[String]'}}
        ChildView(items: items ?? [])
      }
  }
}
