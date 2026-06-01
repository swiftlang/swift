// {"kind":"typecheck","original":"53b0fce0","signature":"swift::rewriting::RequirementMachine::verify(swift::rewriting::MutableTerm const&) const","signatureNext":"RequirementMachine::areReducedTypeParametersEqual"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: Collection
  protocol b {
    associatedtype c
  }
  func d<e, f: b {
    struct g: a {
      typealias Element = f.c
