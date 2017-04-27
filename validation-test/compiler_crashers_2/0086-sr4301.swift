// RUN: not --crash %target-swift-frontend -typecheck -primary-file %s

protocol P {
  init()
}

extension P {
  public init(x: Int, y: Int? = nil) {
    self.init()
  }
}

func foo(t: P.Type, a: Int) {
  let _ = t(x: a)
}
