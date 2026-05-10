// RUN: %batch-code-completion

// https://github.com/apple/swift/issues/56810

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

// CHECK: Decl[InstanceVar]/CurrNominal:      bar[#Bool#];
