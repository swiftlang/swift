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
