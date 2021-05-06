// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

struct Foo {
  let bar: Bool
}

func foo(_: (Foo) -> Void) {}

foo { x in
  switch x.#^COMPLETE_WITHOUT_BRACE?check=CHECK^#
}
foo { x in
  switch x.#^COMPLETE_WITH_BRACES?check=CHECK^# {}
}

// CHECK: Begin completions
// CHECK: Decl[InstanceVar]/CurrNominal:      bar[#Bool#];
// CHECK: End completions
