// {"kind":"complete","languageMode":6,"original":"edae93df","signature":"swift::AnyFunctionType::isSendable() const","signatureAssert":"Assertion failed: (!hasSendableDependentType() && \"Query Sendable dependence first\"), function isSendable"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -swift-version 6 -source-filename %s
withCheckedThrowingContinuation { a in
  Task {
    [b, a.resume]
  }
  #^^#
}
