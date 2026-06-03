// {"kind":"complete","original":"00e5b5d7","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: ((closure->getParent() == DC || closure->getParent()->isChildContextOf(DC)) && \"Decl context isn't correct\"), function walkToClosureExprPre"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@attached(member ) macro a () = {
  @a struct d {
    @b <#declaration#>c { #^^#
  }
}
