// RUN: %swift-ide-test --code-completion --source-filename %s --code-completion-token=COMPLETE | %FileCheck %s

// https://github.com/apple/swift/issues/57061

struct TodaySectionEditView {
    var sections: [SectionHeaderView2] = []
    @ViewBuilder2 var body: some View2 {
        ForEach2(sections, id: \.text) { section in
            Text2("")
            Text2(section.text)
                .#^COMPLETE^#font()
        }
    }
}

protocol View2 {}

extension View2 {
  func font() -> some View2 { fatalError() }
}

@resultBuilder public struct ViewBuilder2 {
  static func buildBlock() -> Never { fatalError() }
  static func buildBlock<Content>(_ content: Content) -> Content where Content : View2 { fatalError() }
  static func buildBlock<C0, C1>(_ c0: C0, _ c1: C1) -> C0 where C0 : View2, C1 : View2 { fatalError() }
}

struct Text2: View2 {
  init(_ s: String) {}
}

struct SectionHeaderView2 {
    let text: String = ""
}

public struct ForEach2<Data, ID, Content>: View2 where Data : RandomAccessCollection {
  init(_ data: Data, id: KeyPath<Data.Element, ID>, @ViewBuilder2 content: @escaping (Data.Element) -> Content) {}
}

// CHECK: Begin completions, 2 items
// CHECK-NEXT: Keyword[self]/CurrNominal:          self[#Text2#];
// CHECK-NEXT: Decl[InstanceMethod]/Super/TypeRelation[Convertible]:         font()[#View2#];
