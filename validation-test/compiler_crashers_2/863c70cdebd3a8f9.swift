// {"kind":"typecheck","signature":"swift::Decl::getSemanticAvailableAttr(swift::AvailableAttr const*) const","signatureAssert":"Assertion failed: (declCtxt->isTypeContext()), function lookupReplacedDecl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  @available(*, renamed : "process0") func a () {
    async {
      a
    }
  }
}
