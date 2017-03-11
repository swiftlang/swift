// RUN: %swift-ide-test -code-completion -code-completion-token=A -source-filename %s
// RUN: %swift-ide-test -code-completion -code-completion-token=B -source-filename %s
// RUN: %swift-ide-test -code-completion -code-completion-token=C -source-filename %s
// RUN: %swift-ide-test -code-completion -code-completion-token=D -source-filename %s
// RUN: %swift-ide-test -code-completion -code-completion-token=E -source-filename %s
// RUN: %swift-ide-test -code-completion -code-completion-token=F -source-filename %s

// Make sure that visible members don't mess up code completion,
// having seen a constructor for an incompatible declaration.

class CompeteInMethod {
  // Here, the method foo is actually visible at the
  // point of code-completion.
  func foo() {
    String(#^A^#
  }
}

class CompleteInVar {
  // Same here - var decls are added to code completion results
  // in a different but similarly shaped code path. So here,
  // the var x is actually visible at the point of code-completion.
  var x: Int {
    String(#^B^#
  }
}

class CompleteOutsideMethod {
  func foo() {}
  init() {
    String(#^C^#
  }
}

class CompleteOutsideVar {
  var x: Int { return 1 }
  init() {
    String(#^D^#
  }
}

class CompleteInsideGenericFunc {
  func foo<S: Sequence>(x: S) {
    String(#^E^#
  }
}

class CompleteInsideGenericClass<S: Sequence> {
  func foo(x: S) {
    String(#^F^#
  }
}
