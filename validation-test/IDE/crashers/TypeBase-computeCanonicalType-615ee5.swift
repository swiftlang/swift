// {"kind":"complete","original":"24b96d5d","signature":"swift::TypeBase::computeCanonicalType()","signatureAssert":"Assertion failed: (Result->isCanonical()), function computeCanonicalType","signatureNext":"TypeBase::isEqual"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a where b == Self
  func c<each d: a>(repeat each d)
  struct e
    subscript -> e {
      #^^#
