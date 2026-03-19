// {"kind":"typecheck","original":"63dee018","signature":"swift::rewriting::RequirementMachine::areReducedTypeParametersEqual(swift::Type, swift::Type) const","signatureNext":"TypeResolution::areSameType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  struct c<d: a> {
    typealias b = d
    enum e where e == <#type#> {
      typealias f = b
    }
    typealias b = d.b
  }
}
