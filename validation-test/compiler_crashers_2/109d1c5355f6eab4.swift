// {"kind":"typecheck","signature":"swift::RequirementSignatureRequest::evaluate(swift::Evaluator&, swift::ProtocolDecl*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a : b where c == d<e> protocol f : g protocol g {
  associatedtype 2 : a
}
struct d < e extension d
    : f protocol h{associatedtype e associatedtype c : g} protocol b : h
