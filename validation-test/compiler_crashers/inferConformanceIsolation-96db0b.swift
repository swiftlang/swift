// {"frontendArgs":["-typecheck","-enable-upcoming-feature","InferIsolatedConformances","-enable-experimental-feature","Reparenting"],"kind":"custom","signature":"swift::inferConformanceIsolation(swift::NormalProtocolConformance*, bool)","signatureAssert":"Assertion failed: (!isa<ProtocolDecl>(this) && \"For inherited protocols, use ProtocolDecl::inheritsFrom() or \" \"ProtocolDecl::getInheritedProtocols()\"), function getAllProtocols","signatureNext":"ConformanceIsolationRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck -enable-upcoming-feature InferIsolatedConformances -enable-experimental-feature Reparenting %s
// REQUIRES: swift_feature_InferIsolatedConformances
// REQUIRES: swift_feature_Reparenting
@reparentable
protocol P {
}
protocol Q: P {
}
extension Q: @reparented P {
}
