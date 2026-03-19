// {"kind":"typecheck","original":"e6befa86","signature":"swift::rewriting::RequirementMachine::verify(swift::rewriting::MutableTerm const&) const","signatureNext":"RequirementMachine::getReducedShapeTerm"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b> {
  func
    c<each d>(e: repeat each d) where repeat (b) == (each d, Void)
  {
    repeat e
  }
}
