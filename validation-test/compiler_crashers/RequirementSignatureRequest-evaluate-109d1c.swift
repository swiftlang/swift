// {"kind":"typecheck","signature":"swift::RequirementSignatureRequest::evaluate(swift::Evaluator&, swift::ProtocolDecl*) const","signatureNext":"ProtocolDecl::getRequirementSignature"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a : b where c == d<e> protocol f : g protocol g {
  associatedtype 2 : a
}
struct d < e extension d
    : f protocol h{associatedtype e associatedtype c : g} protocol b : h
