// {"kind":"emit-silgen","signature":"swift::Lowering::SILGenFunction::emitProtocolWitness(swift::Lowering::AbstractionPattern, swift::CanTypeWrapper<swift::AnyFunctionType>, swift::SILDeclRef, swift::SubstitutionMap, swift::SILDeclRef, swift::SubstitutionMap, swift::Lowering::IsFreeFunctionWitness_t, bool, bool, std::__1::optional<swift::ActorIsolation>)","signatureAssert":"Assertion failed: (value->getType() == SILType::getOpaqueIsolationType(getASTContext()) && \"Can only convert Optional<any Actor> to \" \"Builtin.ImplicitActor\"), function convertToImplicitActor","signatureNext":"Lowering::SILGenModule::emitProtocolWitness"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol P: Actor {
  func f() async
}
actor A: P {
  nonisolated(nonsending) func f() async {
  }
}
