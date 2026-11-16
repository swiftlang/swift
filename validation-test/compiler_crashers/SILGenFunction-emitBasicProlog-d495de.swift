// {"kind":"emit-ir","signature":"swift::Lowering::SILGenFunction::emitBasicProlog(swift::DeclContext*, swift::ParameterList*, swift::ParamDecl*, swift::Type, std::__1::optional<swift::Type>, swift::SourceLoc, unsigned int)","signatureAssert":"Assertion failed: (isFinished() && \"didn't finish generating the collection\"), function finish","signatureNext":"Lowering::SILGenFunction::emitProlog"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
@_silgen_name("c")
func a() {
}
@_silgen_name("c")
func b(_: Int32)
b(0)
