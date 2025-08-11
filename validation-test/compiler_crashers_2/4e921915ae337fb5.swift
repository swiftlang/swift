// {"kind":"typecheck","signature":"swift::ConditionalRequirementsRequest::evaluate(swift::Evaluator&, swift::NormalProtocolConformance*) const","signatureAssert":"Assertion failed: (typeSig.getCanonicalSignature().getGenericParams() == extensionSig.getCanonicalSignature().getGenericParams()), function evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a typealias b<c> = () extension b : a
