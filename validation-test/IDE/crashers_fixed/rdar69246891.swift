// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE_2 -source-filename=%s

class MyCls {
  public init(body: (Int) throws -> Void) {}
}

func foo() {
  MyCls { arg in
    MyCls { #^COMPLETE^#
    }
  }
}

func receive(fn: () -> throws Void) {}

func test2() {
  receive {
    switch #^COMPLETE_2^#
  }
}
