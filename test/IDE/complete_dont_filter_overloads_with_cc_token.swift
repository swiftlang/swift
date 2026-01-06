// RUN: %batch-code-completion

func buildView<T>(@MyViewBuilder _ x: () -> T) {}

@resultBuilder struct MyViewBuilder {
  static func buildBlock(_ content: MyText) -> MyText { content }
}

struct MyText : Equatable {
  init(verbatim content: String) {}
  init<S>(_ content: S) where S : StringProtocol {}
}

func test(text: String) {
  buildView {
    MyText(#^COMPLETE^#text)
  }
}

// COMPLETE: Begin completions
// COMPLETE-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ['(']{#verbatim: String#}[')'][#MyText#];
// COMPLETE-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ['(']{#(content): StringProtocol#}[')'][#MyText#];
// COMPLETE-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: text[#String#];
// COMPLETE: End completions
