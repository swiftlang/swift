// {"kind":"typecheck","original":"379a5956","signature":"swift::rewriting::RequirementMachine::isConcreteType(swift::Type, swift::ProtocolDecl const*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b: c where b.d == b
  protocol c {
    associatedtype d: a where d.b == Self
  }
}
