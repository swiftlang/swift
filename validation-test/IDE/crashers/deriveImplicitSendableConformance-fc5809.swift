// {"kind":"complete","original":"0198d829","signature":"swift::deriveImplicitSendableConformance(swift::Evaluator&, swift::NominalTypeDecl*)","signatureAssert":"Assertion failed: (!isa<SendableAttr>(attr) && \"Conformance should have been added by SynthesizedProtocolAttr!\"), function deriveImplicitSendableConformance"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@Sendable struct a {
  #^^#
}
