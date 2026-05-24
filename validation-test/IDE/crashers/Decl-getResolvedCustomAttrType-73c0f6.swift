// {"kind":"complete","original":"0ba46c61","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: (!isa<SendableAttr>(attr) && \"Conformance should have been added by SynthesizedProtocolAttr!\"), function deriveImplicitSendableConformance"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a {
  #^^#
}
@resultBuilder struct b<c> where c: Sendable
  @Sendable struct d
    func e(@b<d>  ())
    let f = e {
