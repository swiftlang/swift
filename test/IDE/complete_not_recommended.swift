// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER | %FileCheck %s -check-prefix=CHECK1
// CHECK1: Begin completions, 3 items
// CHECK1: Keyword[self]/CurrNominal:          self[#A.Type#]; name=self
// CHECK1: Decl[InstanceMethod]/CurrNominal:   foo({#self: A#})[#() -> Void#]
// CHECK1: Decl[Constructor]/CurrNominal:      init()[#A#]; name=init(){{$}}
// CHECK1: End completions

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERRIDE_1 | %FileCheck %s -check-prefix=OVERRIDE_1

class A {
  func foo() {}
  @available(*, unavailable) func unavail() {}
}

func glob() {
  A.#^MEMBER^#
}

class B : A {
  override func #^OVERRIDE_1^#
// OVERRIDE_1: Begin completions
// OVERRIDE_1-NOT: Decl[InstanceMethod]
// OVERRIDE_1: Decl[InstanceMethod]/Super:         foo() {|};
// OVERRIDE_1-NOT: Decl[InstanceMethod]
// OVERRIDE_1: End completions
}
