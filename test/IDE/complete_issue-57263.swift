// RUN: %swift-ide-test -code-completion -code-completion-token COMPLETE -source-filename %s | %FileCheck %s

// https://github.com/apple/swift/issues/57263

struct Foo {
	var bar: Int
}

protocol View2 {}
struct EmptyView: View2 {}

@resultBuilder public struct ViewBuilder2 {
  public static func buildBlock(_ content: EmptyView) -> EmptyView { fatalError() }
}

public struct List2 {
    public init(selection: Int?, @ViewBuilder2 content: () -> EmptyView)
    public init(selection: String?, @ViewBuilder2 content: () -> EmptyView)
}

func foo(kp: (Foo) -> String) {}

func foo() {
    List2 {
        foo(kp: \.self#^COMPLETE^#)
// CHECK:      Begin completions, 1 items
// CHECK-NEXT: Decl[InstanceVar]/CurrNominal:      .bar[#Int#];
    }
    .unknownMethod()
}
