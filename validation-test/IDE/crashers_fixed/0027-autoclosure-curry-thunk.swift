// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE

struct MyView {
  func pnAppear(perform action: (() -> Void)) -> MyView {
    return self
  }
  func qadding(_ edges: Int, _ length: Int) -> MyView {
    return self
  }
}

@resultBuilder struct ViewBuilder {
  static func buildBlock(_ content: MyView) -> MyView {
    return content
  }
}

struct AdaptsToSoftwareKeyboard {
  @ViewBuilder func body(content: MyView) -> MyView {
    content
      .qadding(1234567, #^COMPLETE^#0)
      .pnAppear(perform: subscribeToKeyboardEvents)
  }

  func subscribeToKeyboardEvents() {}
}
