// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE

@resultBuilder public struct ViewBuilder {
  static func buildBlock(_ content: TextField) -> TextField { fatalError() }
}

struct TextField {
  init(_ title: String, text: String) {}
}

struct EncodedView {
    @ViewBuilder var body: TextField {
      TextField("Encoded", #^COMPLETE^#)
    }
}
