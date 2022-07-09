// RUN: %target-swift-frontend -typecheck -verify %s

struct S {
  private let data: [[String]]
  private func f() {}

  func test() {
    // expected-error@+1 {{static method 'buildBlock' requires that 'ForEach<[String], ()>' conform to 'View'}}
    ForEach(data) { group in
      ForEach(group) { month in
        self.f()
      }
    }
  }
}

struct Wrapper<T> {}

protocol View {}

@resultBuilder struct Builder {
  // expected-note@+1 {{where 'Content' = 'ForEach<[String], ()>'}}
  static func buildBlock<Content: View>(_ content: Content) -> Content { fatalError() }
}

struct ForEach<Data, Content> where Data : RandomAccessCollection {
  init<C>(_ data: Wrapper<C>, @Builder content: (Wrapper<C.Element>) -> Content) where C : MutableCollection {}
  init(_ data: Data, @Builder content: @escaping (Data.Element) -> Content) {}
}
