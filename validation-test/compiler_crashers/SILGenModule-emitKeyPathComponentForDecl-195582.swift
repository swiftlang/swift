// {"kind":"emit-silgen","original":"4a74f554","signature":"swift::Lowering::SILGenModule::emitKeyPathComponentForDecl(swift::SILLocation, swift::GenericEnvironment*, swift::ResilienceExpansion, unsigned int&, bool&, swift::SubstitutionMap, swift::ValueDecl*, llvm::ArrayRef<swift::ProtocolConformanceRef>, swift::CanType, swift::DeclContext*, bool, bool)","signatureAssert":"Assertion failed: (!componentTy->hasTypeParameter()), function emitKeyPathComponentForDecl","signatureNext":"RValueEmitter::visitKeyPathExpr"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  associatedtype b
  var c: b {
    get
  }
}
func d() {
  let e: (a) -> Any = \.c
}
