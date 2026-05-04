// {"kind":"typecheck","original":"509f8079","signature":"swift::AbstractSpecializeAttr::getSpecializedSignature(swift::AbstractFunctionDecl const*) const","signatureAssert":"Assertion failed: (declCtxt->isTypeContext()), function lookupReplacedDecl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a < b >() {
  @_specialize(target : c where b == <#type#>) func d()
}
