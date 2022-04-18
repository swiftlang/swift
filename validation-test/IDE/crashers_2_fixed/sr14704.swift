// RUN: %swift-ide-test --code-completion --source-filename %s --code-completion-token=COMPLETE

@resultBuilder struct ViewBuilder2 {
  static func buildBlock() -> Never { fatalError() }
  static func buildBlock<Content>(_ content: Content) -> Content where Content : View2 { fatalError() }
}

protocol View2 {}

extension View2 {
  func qadding(_ edges: Set2) -> some View2 { fatalError() }
  func pnAppear(perform action: (() -> Void)? = nil) -> some View2 { fatalError() }
}

struct EmptyView2: View2 {}

struct Set2 {
  static let bottom = Set2()
}


struct AdaptsToSoftwareKeyboard {

  @ViewBuilder2 func body(content: EmptyView2) -> some View2 {
      content
            .qadding(.#^COMPLETE^#bottom)
            .pnAppear(perform: subscribeToKeyboardEvents)
    }

    private func subscribeToKeyboardEvents() {}
}
