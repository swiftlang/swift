// {"kind":"typecheck","original":"7d066aab","signature":"swift::rewriting::RequirementMachine::isConcreteType(swift::Type, swift::ProtocolDecl const*) const","signatureNext":"RequirementSignatureRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b: a where b.c == Self
  associatedtype c: d where c.e == Self
}
protocol d {
  associatedtype e: a where e.c == Self
}
