// {"kind":"typecheck","original":"25bd9966","signature":"swift::rewriting::RequirementMachine::lookupNestedType(swift::Type, swift::Identifier) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b: c
}
extension a {
  protocol d {
    #e(    == g
  }
  class c<f
    extension a where b: b.e
