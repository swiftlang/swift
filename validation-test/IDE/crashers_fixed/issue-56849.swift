// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s | %FileCheck %s

// https://github.com/apple/swift/issues/56849

protocol MyView {
    associatedtype Body : MyView
    @MyViewBuilder var body: Self.Body { get }
}

@resultBuilder public struct MyViewBuilder {
    static func buildBlock() -> MyZStack<Never> { fatalError() }
    static func buildBlock<Content>(_ content: Content) -> Content { content }
}

struct MyAlignment {
  static let center: MyAlignment
}

struct MyZStack<Content> : MyView {
    init(alignment: MyAlignment, @MyViewBuilder content: () -> Content) {
        fatalError()
    }

    func my_updating<State>(body: (inout State) -> Void) {}
}

struct BottomMenu: MyView {
    var body: some MyView {
        let a = MyZStack(alignment: .#^COMPLETE^#center, content: {})
        .my_updating(body: { state in
            state = false
        })
    }
}

// CHECK: Begin completions, 2 items
// CHECK: Decl[StaticVar]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: center[#MyAlignment#]; name=center
// CHECK: Decl[Constructor]/CurrNominal/TypeRelation[Convertible]: init()[#MyAlignment#]; name=init()
