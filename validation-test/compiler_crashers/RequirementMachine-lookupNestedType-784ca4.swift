// {"kind":"typecheck","original":"a43d8500","signature":"swift::rewriting::RequirementMachine::lookupNestedType(swift::Type, swift::Identifier) const","signatureNext":"TypeResolver::resolveDependentMemberType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  struct c
    typealias d = c where d == b.e
