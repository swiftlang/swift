// RUN: %swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

struct Image {}

@resultBuilder struct ViewBuilder2 {
  static func buildBlock<Content>(_ content: Content) -> Content { fatalError() }
}

struct NavigationLink<Destination> {
    init(@ViewBuilder2 destination: () -> Destination)
    init(_ title: String)
}

struct CollectionRowView {
    func foo() {
        NavigationLink() {
            Image().#^COMPLETE^#
