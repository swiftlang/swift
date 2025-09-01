// RUN: %empty-directory(%t)
// RUN: %batch-code-completion -debug-constraints 2> %t/constraints.log
// RUN: %FileCheck %s -check-prefix CONSTRAINTS < %t/constraints.log

struct S {
  func bar(_ x: Int) {}
  func bar(_ x: String) {}
}

@available(*, unavailable)
func foo(_ fn: (S) -> Void) {}
func foo(_ x: () -> Void) {}

// Make sure we can try the unavailable overload if necessary.
foo {
  $0.#^COMPLETE1^#
  // COMPLETE1: Decl[InstanceMethod]/CurrNominal: bar({#(x): Int#})[#Void#]; name=bar(:)
  // COMPLETE1: Decl[InstanceMethod]/CurrNominal: bar({#(x): String#})[#Void#]; name=bar(:)
}

func baz(_ fn: (S) -> Void) {}

@available(*, unavailable)
func baz(_ x: () -> Void) {}

// Here we can prune the unavailable overload of baz.
baz {
  $0.#^COMPLETE2^#
  // COMPLETE2: Decl[InstanceMethod]/CurrNominal: bar({#(x): Int#})[#Void#]; name=bar(:)
  // COMPLETE2: Decl[InstanceMethod]/CurrNominal: bar({#(x): String#})[#Void#]; name=bar(:)
}
// CONSTRAINTS: attempting disjunction choice {{.*}}:21:6
// CONSTRAINTS: skipping unavailable disjunction choice {{.*}}:24:6
