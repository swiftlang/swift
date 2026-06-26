// {"kind":"typecheck","original":"0b1395b1","signature":"swift::rewriting::RequirementMachine::lookupNestedType(swift::Type, swift::Identifier) const","signatureNext":"TypeResolver::resolveDeclRefTypeReprRec"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  typealias c = b.d where e == c
}
