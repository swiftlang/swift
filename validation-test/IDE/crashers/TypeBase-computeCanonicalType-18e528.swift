// {"kind":"complete","original":"2c7b09da","signature":"swift::TypeBase::computeCanonicalType()","signatureAssert":"Assertion failed: (Result->isCanonical()), function computeCanonicalType","signatureNext":"Mangle::ASTMangler::getDeclTypeForMangling"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a where b == Self
  func c<each d: a>(repeat each d)
  #^^#
