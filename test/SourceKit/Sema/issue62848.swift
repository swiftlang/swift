// RUN: %sourcekitd-test -req=collect-type %s -- %s

struct Rectangle {
  init() {}
}

@resultBuilder public struct WiewBuilder {
  public static func buildBlock<Content>(_ content: Content) -> Content {
    return content
  }
  public static func buildIf<Content>(_ content: Content?) -> Content? {
    return content
  }
}

public struct AStack<Content> {
  init(@WiewBuilder content: () -> Content) {}
}

func foo() {
  AStack {
    if true {
      Rectangle()
