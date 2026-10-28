// {"kind":"typecheck","signature":"swift::rewriting::RequirementMachine::verify(swift::rewriting::MutableTerm const&) const","signatureNext":"RequirementMachine::getReducedShapeTerm"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  struct b < each c {
    extension a {
      struct d {
        e(repeat(each c))
