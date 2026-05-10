// {"kind":"emit-silgen","original":"63bac84b","signature":"getBaseAccessKind(swift::Lowering::SILGenModule&, swift::AbstractStorageDecl*, swift::Lowering::SGFAccessKind, swift::AccessStrategy, swift::CanType, bool)","signatureNext":"SILGenLValue::visitMemberRefExpr"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
struct a: ~Copyable {
}
@propertyWrapper
struct b: ~Copyable {
  var wrappedValue: a
}
struct c: ~Copyable {
  @b var d: a
  func e() {
    d
  }
}
