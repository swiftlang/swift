// {"kind":"complete","original":"582d6872","signature":"swift::ide::getSelectedOverloadInfo(swift::constraints::Solution const&, swift::constraints::ConstraintLocator*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ArgumentTypeCheckCompletionCallback::sawSolutionImpl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@dynamicMemberLookup struct a<b> {
  subscript<c>(dynamicMember d: KeyPath<b, c> & Sendable) -> c
  func e(f: a<[Int]>) {
    f[#^^#]
  }
}
