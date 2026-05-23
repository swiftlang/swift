// {"kind":"complete","original":"e052a31c","signature":"swift::TypeBase::getSuperclassForDecl(swift::ClassDecl const*, bool)","signatureAssert":"Assertion failed: (nominalDecl && \"expected nominal type here\"), function getSuperclassForDecl","signatureNext":"TypeBase::getContextSubstitutions"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@propertyWrapper
class a<b> {
  var wrappedValue: b
  var projectedValue: a
  init(projectedValue: <#type#>)
}
func c(@a : <#type#>)
} c($d: #^^#
