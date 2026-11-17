// {"extraArgs":["-solver-disable-diagnose-valid-salvage"],"kind":"typecheck","original":"4c66af68","signature":"swift::TypeChecker::typeCheckTarget(swift::constraints::SyntacticElementTarget&, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>, swift::DiagnosticTransaction*)","signatureNext":"StmtChecker::typeCheckStmt"}
// RUN: not --crash %target-swift-frontend -typecheck -solver-disable-diagnose-valid-salvage %s
protocol a {
  associatedtype b
}
protocol c {
  associatedtype d: a
}
protocol e {
  associatedtype f: c
  func p() -> f
}
struct g<h> {
  class i<j: c>: c {
    init(n: j, k: (j) -> j.d.b)
  }
  func l<m: e>(o: m) -> c where m.f.d == g<m> {
    i(n: o.p()) {
      $0
    }
  }
}
