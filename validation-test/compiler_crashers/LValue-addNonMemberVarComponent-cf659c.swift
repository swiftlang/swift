// {"kind":"emit-silgen","original":"1ba21848","signature":"swift::Lowering::LValue::addNonMemberVarComponent(swift::Lowering::SILGenFunction&, swift::SILLocation, swift::VarDecl*, swift::SubstitutionMap, swift::Lowering::LValueOptions, swift::Lowering::SGFAccessKind, swift::AccessStrategy, swift::CanType, std::__1::optional<swift::ActorIsolation>)","signatureNext":"emitLValueForNonMemberVarDecl"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@propertyWrapper struct a<b> {
  init(wrappedValue: b) {
  }
  var wrappedValue: b {
  }
  var projectedValue: a {
    get {
    }
    set {
    }
  }
}
func c<b>(@a d: b) {
}
