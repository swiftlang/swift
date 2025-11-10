// {"kind":"complete","original":"081c2cc3","signature":"abortWithVerificationError"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b: SignedInteger
    c,
    : {
    }
}
extension a {
  d: SignedInteger, #^^#
