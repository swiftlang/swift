// {"frontendArgs":["-emit-silgen","-parse-stdlib"],"kind":"custom","signature":"swift::SILType::getOpaqueIsolationType(swift::ASTContext const&)","signatureNext":"Lowering::SILGenFunction::emitExpectedExecutorProlog"}
// RUN: not --crash %target-swift-frontend -emit-silgen -parse-stdlib %s
func a() async {
}
