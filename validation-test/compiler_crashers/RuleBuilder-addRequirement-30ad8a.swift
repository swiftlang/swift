// {"kind":"typecheck","original":"3a09b141","signature":"swift::rewriting::RuleBuilder::addRequirement(swift::Requirement const&, swift::ProtocolDecl const*, std::__1::optional<llvm::ArrayRef<swift::rewriting::Term>>)","signatureAssert":"Assertion failed: (otherType->isParameterPack()), function addRequirement","signatureNext":"RuleBuilder::initWithConditionalRequirements"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b : a
}
struct c < each d
  extension c : a where repeat each d : a,
  (repeat (each d).b) == (repeat each d) {
    typealias e = c
    func g < f : a where f.b == e
