// RUN: %batch-code-completion

// https://github.com/apple/swift/issues/78397

struct TabView<Content> {
  public init(@ViewBuilder content: () -> Content)
}

protocol View {}
func tabItem() -> some View

@resultBuilder struct ViewBuilder {
  static func buildBlock<T>(_ content: T) -> T
}

func test() -> some View {
  TabView {
    tabItem
  }#^COMPLETE^#
  // COMPLETE: Begin completions
}
