// {"kind":"typecheck","original":"25bd9966","signature":"swift::rewriting::RequirementMachine::lookupNestedType(swift::Type, swift::Identifier) const"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b: Error
}
extension a {
  protocol c {
    #d(    == e
  }
  class Error where b: c, b.d:
