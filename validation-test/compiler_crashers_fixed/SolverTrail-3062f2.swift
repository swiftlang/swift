// {"kind":"typecheck","original":"f7852f7c","signature":"swift::constraints::SolverTrail::~SolverTrail()","signatureAssert":"Assertion failed: (Changes.empty() && \"Trail corrupted\"), function ~SolverTrail","signatureNext":"ConstraintSystem::SolverState"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a
  struct b<c {
    d: c
    var e: a
  }
  func f<c>(b    < c >)
    f(b (d:
    ""
  e:  ""
