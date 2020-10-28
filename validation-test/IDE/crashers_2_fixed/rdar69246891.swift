// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

class MyCls {
  public init(body: (Int) throws -> Void) {}
}

func foo() {
  MyCls { arg in
    MyCls { #^COMPLETE^#
    }
  }
}
