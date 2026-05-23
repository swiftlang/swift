// {"kind":"complete","languageMode":6,"original":"7a753bb2","signature":"swift::constraints::ConstraintSystem::applySolution(swift::constraints::Solution&, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (isValidType(solution.simplifyType(type)) && \"node type has invalid type\"), function applySolution","signatureNext":"TypeChecker::typeCheckTarget"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -swift-version 6 -source-filename %s
struct a {
  struct b<c> {
    var d: (c) -> Bool
  }
  var e = b<Int> {
    f in
    func g() {
      switch <#expression#> {
      case #^^#:
        nil
      }
    }
  }
}
