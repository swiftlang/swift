// {"frontendArgs":["-typecheck","-parse-stdlib"],"kind":"custom","signature":"swift::checkConformance(swift::Type, swift::ProtocolDecl*, bool)","signatureNext":"DiagnoseWalker::walkToExprPre"}
// RUN: not --crash %target-swift-frontend -typecheck -parse-stdlib %s
func f(x: ()) {
  _ = consume x
}
