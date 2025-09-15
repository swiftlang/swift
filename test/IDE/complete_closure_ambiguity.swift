// RUN: %empty-directory(%t)
// RUN: %batch-code-completion -debug-constraints 2> %t/constraints.log
// RUN: %FileCheck %s -check-prefix CONSTRAINTS < %t/constraints.log
// RUN: %FileCheck %s -check-prefix CONSTRAINTS-NOT < %t/constraints.log

protocol P1 {}
protocol P2 {}

func foo<T: P1>(_ fn: () -> T) {}

@_disfavoredOverload
func foo<T: P2>(_ fn: () -> T) {}

func bar(_ x: Int) -> Int {}
func bar(_ x: String) -> String {}

// Make sure we eagerly prune the disfavored overload of 'foo', despite the
// ambiguity in the closure body.
foo {
  let x = bar(#^COMPLETE^#)
  // COMPLETE: Decl[FreeFunction]/CurrModule/Flair[ArgLabels]: ['(']{#(x): Int#}[')'][#Int#]; name=:
  // COMPLETE: Decl[FreeFunction]/CurrModule/Flair[ArgLabels]: ['(']{#(x): String#}[')'][#String#]; name=:

  return x
}

// CONSTRAINTS: attempting disjunction choice {{.*}}:9:6
// CONSTRAINTS: skipping disfavored disjunction choice {{.*}}:12:6

// CONSTRAINTS-NOT-NOT: increasing 'hole'
