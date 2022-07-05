// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE

@resultBuilder public struct WiewBuilder {
  static func buildBlock<T>(_ content: T) -> T { return content }
}

func pnAppear(perform action: () -> Void) {}

public func asyncAfter(execute work: () -> Void) {}

struct ProgressView {
  @WiewBuilder var body: Void {
    pnAppear(perform: {
      #^COMPLETE^#asyncAfter() {}
    })
  }
}
