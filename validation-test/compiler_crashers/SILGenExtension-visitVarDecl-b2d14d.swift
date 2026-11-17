// {"kind":"emit-silgen","signature":"SILGenExtension::visitVarDecl(swift::VarDecl*)","signatureAssert":"Assertion failed: ((vd->isStatic() || hasDidSetOrWillSetDynamicReplacement) && \"stored property in extension?!\"), function visitVarDecl","signatureNext":"SILGenExtension::emitExtension"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
struct a {
}
extension a {
  @_dynamicReplacement(for:b) var c: Int
}
