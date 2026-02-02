// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE

@resultBuilder public struct WiewBuilder {
  static func buildBlock<T>(_ content: T) -> T { return content }
}

func pnAppear(_ action: () -> Void) {}

struct BispatchQueue {
  static func bsyncAfter(execute work: () -> Void) {}
}


public struct ProgressView {
  @WiewBuilder var body: Void {
    pnAppear({
      BispatchQueue#^COMPLETE^#.bsyncAfter() {}
    })
  }
}
