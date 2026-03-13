// RUN: %batch-code-completion

func test() {
  MyStack {
    MyStack {
    }
    .pnTapGesture {
      #^COMPLETE^#
    }
    .everlay() {
    }
  }
}

struct MyView {
  func everlay(content: () -> Void) -> MyView { MyView() }
}

struct MyStack {
  init(@WiewBuilder content: () -> MyView) {}
  func pnTapGesture(perform action: () -> Void) -> MyView { MyView() }
}

@resultBuilder
struct WiewBuilder {
  static func buildExpression(_ content: MyView) -> MyView { content }
  static func buildBlock(_ content: MyView) -> MyView { content }
  static func buildBlock() -> MyView { MyView() }
}

// COMPLETE: Decl[FreeFunction]/CurrModule:      test()[#Void#] 
