// {"extraArgs":["-language-mode","6"],"kind":"complete","original":"edae93df","signature":"swift::AnyFunctionType::isSendable() const","signatureAssert":"Assertion failed: (!hasTypeDependencyFor(TypeDependenceKind::Sendable) && \"Query Sendable dependence first\"), function isSendable","signatureNext":"getBuiltinFunctionTypeConformance"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -swift-version 6 -source-filename %s
withCheckedThrowingContinuation { a in
  Task {
    [b, a.resume]
  }
  #^^#
}
