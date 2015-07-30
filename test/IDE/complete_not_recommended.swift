// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER | FileCheck %s -check-prefix=CHECK1
// CHECK1: Begin completions, 2 items
// CHECK1: Decl[InstanceMethod]/CurrNominal:   foo({#self: A#})[#() -> Void#]
// CHECK1: Decl[Constructor]/CurrNominal:      init()[#A#]; name=init(){{$}}
// CHECK1: End completions

class A {
  func foo() {}
  @available(*, unavailable) func unavail() {}
}

func glob() {
  A.#^MEMBER^#
}
