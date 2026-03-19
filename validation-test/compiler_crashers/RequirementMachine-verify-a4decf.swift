// {"kind":"typecheck","original":"926d0cea","signature":"swift::rewriting::RequirementMachine::verify(swift::rewriting::MutableTerm const&) const","signatureNext":"RequirementMachine::getSuperclassBound"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b: c where b.d == Self
  associatedtype e: c where e.d == Self
  func compose<f>() -> i<Self, f>
  protocol c {
    associatedtype d
  }
  struct i<g: a, h: a>: a where g.e == h.b {
  }
}
