
protocol View2 {}

@resultBuilder public struct ViewBuilder2 {
  static func buildBlock<Content>(_ content: Content) -> Content where Content : View2 { fatalError() }
  static func buildBlock<C0, C1>(_ c0: C0, _ c1: C1) -> C0 where C0 : View2, C1: View2 { fatalError() }
}

struct MyText: View2 {}

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token SINGLE_ELEMENT | %FileCheck %s --check-prefix=SINGLE_ELEMENT
struct MyView {
	@ViewBuilder2 var body2: some View2 {
		#^SINGLE_ELEMENT^#
	}
}
// SINGLE_ELEMENT: Begin completions
// SINGLE_ELEMENT-DAG: Decl[Struct]/CurrModule/TypeRelation[Convertible]: MyText[#MyText#];
// SINGLE_ELEMENT: End completions

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token SECOND_ELEMENT | %FileCheck %s --check-prefix=SECOND_ELEMENT
struct MyView {
	@ViewBuilder2 var body2: some View2 {
		MyText()
		#^SECOND_ELEMENT^#
	}
}
// SECOND_ELEMENT: Begin completions
// SECOND_ELEMENT-DAG: Decl[Struct]/CurrModule/TypeRelation[Convertible]: MyText[#MyText#];
// SECOND_ELEMENT: End completions
