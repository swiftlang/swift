// RUN: %batch-code-completion

struct Route {
  func makeDetailView() -> Int {
    return 52
  }
}

@resultBuilder struct ViewBuilder {
    static func buildBlock(_ x: Int) -> Int { return x }
}

func foo(@ViewBuilder destination: () -> Int) {}
func foo(destination: Int) {}

func test(route: Route?) {
  route.map { route in
    foo(destination: route.#^COMPLETE^#)
  }
}


// COMPLETE-DAG: Keyword[self]/CurrNominal:          self[#Route#];
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: makeDetailView()[#Int#];

func testTask(route: Route) {
  Task {
    route.#^IN_TASK?check=COMPLETE^#
  }
}

protocol Builder {
  func recv<T>(_: (T) throws -> Void)
}

struct S {
  func foo(x: Int) throws {}
  func test(builder: Builder) {
    builder.recv(#^IN_CONTEXT_FOR_UNAPPLIED_REF^#)
  }
}
// IN_CONTEXT_FOR_UNAPPLIED_REF-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: foo(x:)[#(Int) throws -> ()#]; name=foo(x:)
// IN_CONTEXT_FOR_UNAPPLIED_REF-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: test(builder:)[#(any Builder) -> ()#]; name=test(builder:)
