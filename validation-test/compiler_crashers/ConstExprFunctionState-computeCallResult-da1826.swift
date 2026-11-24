// {"extraArgs":["-enable-experimental-feature","StaticAssert"],"kind":"emit-sil","signature":"swift::ConstExprFunctionState::computeCallResult(swift::ApplyInst*)","signatureAssert":"Assertion failed: (!calleeFnType->hasSelfParam() || !calleeFnType ->getSelfInstanceType( callee->getModule(), apply->getFunction()->getTypeExpansionContext()) ->getClassDecl() && \"class methods are not supported\"), function computeCallResult","signatureNext":"ConstExprFunctionState::evaluateFlowSensitive"}
// RUN: not --crash %target-swift-frontend -emit-sil -enable-experimental-feature StaticAssert %s
// REQUIRES: swift_feature_StaticAssert
class a {
  var b = 2
}
func c() -> Int {
  a().b
}
func d() {
  #assert(c() == 2)
}
