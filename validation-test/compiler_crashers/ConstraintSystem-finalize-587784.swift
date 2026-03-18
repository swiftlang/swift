// {"kind":"typecheck","original":"8d3173cf","signature":"swift::constraints::ConstraintSystem::finalize()","signatureNext":"SplitterStep::mergePartialSolutions"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b(c: Bool) -> some a {
    &
    if c {
      return d(2.0
      1.0
