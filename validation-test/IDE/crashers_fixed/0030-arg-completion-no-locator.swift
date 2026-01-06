// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE

@dynamicMemberLookup public struct Binding<Value> {
    subscript<Subject>(dynamicMember keyPath: WritableKeyPath<Value, Subject>) -> Binding<Subject> {
      fatalError()
    }
}

struct Foo {
  var bar: Binding<[String]>

  func test(index: Int) {
    _ = bar[#^COMPLETE^#index]
  }
}
