// RUN: %target-swift-frontend -typecheck -verify %s

// https://github.com/apple/swift/issues/57241

struct S {
  private let data: [[String]]
  private func f() {}

  func test() {
    ForEach(data) { group in
      ForEach(group) { month in
        self.f()
        // expected-error@-1 {{type '()' cannot conform to 'View'}}
        // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
      }
    }
  }
}

struct Wrapper<T> {}

protocol View {}

@resultBuilder struct Builder {
  // expected-note@+1 {{required by static method 'buildBlock' where 'Content' = '()'}}
  static func buildBlock<Content: View>(_ content: Content) -> Content { fatalError() }
}

struct ForEach<Data, Content> where Data : RandomAccessCollection {
  init<C>(_ data: Wrapper<C>, @Builder content: (Wrapper<C.Element>) -> Content) where C : MutableCollection {}
  init(_ data: Data, @Builder content: @escaping (Data.Element) -> Content) {}
}
