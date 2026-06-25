// {"kind":"complete","original":"a977b385","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: (genericSig->isConcreteType(gp)), function applyUnboundGenericArguments"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@propertyWrapper struct a {
  struct b<c> {
    typealias d<e, f> = a
  }
  #^^#@b.d<String, Int>  var g
}
