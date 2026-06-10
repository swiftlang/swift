// {"kind":"emit-silgen","original":"49219174","signature":"(anonymous namespace)::AccessEmitter<swift::Lowering::LValue::addMemberVarComponent(swift::Lowering::SILGenFunction&, swift::SILLocation, swift::VarDecl*, swift::SubstitutionMap, swift::Lowering::LValueOptions, bool, swift::Lowering::SGFAccessKind, swift::AccessStrategy, swift::CanType, bool, std::__1::optional<swift::ActorIsolation>)::MemberVarAccessEmitter, swift::VarDecl>::emitUsingAccessor(swift::AccessorKind, bool)","signatureAssert":"Assertion failed: (!ActorIso), function emitUsingCoroutineAccessor","signatureNext":"Lowering::LValue::addMemberVarComponent"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@globalActor struct a {
  actor b {
  }
  static let shared = b()
}
@a final class c {
  var d: Int {
    _read {
    }
  }
}
func e(f: c) async {
  async let g = f.d
}
