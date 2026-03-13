// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE | %FileCheck %s

protocol View {}

@resultBuilder
struct ViewBuilder {
  static func buildBlock(_ component: MyView) -> MyView {
    MyView()
  }
}

struct QStack<Content> {
  init(@ViewBuilder content: () -> Content) {}
  
  func qheet<Content>(@ViewBuilder content: @escaping () -> Content) -> some View {
    MyView()
  }
}

struct MyView : View {
  func qag<V>(_ tag: V) -> MyView {
    MyView()
  }
}


func foo() {
  QStack {
    // When solving for code completion, the constraint system doesn't increase the score for non-default literals.
    // This causes ambiguous results for this qag call if it's type checked for code completion, preventing results from showing up at the code completion token.
    // Solution is to do normal type checking for syntactic elements that don't contain the code completion token.
    MyView().qag(0)
  }
  .qheet() {
    MyView().#^COMPLETE^#
  }
}
// CHECK: Begin completions, 2 items
// CHECK-DAG: Keyword[self]/CurrNominal:          self[#MyView#];
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: qag({#(tag): V#})[#MyView#];
