// {"kind":"complete","original":"098f0ede","signature":"swift::ide::CodeCompletionStringBuilder::addTypeAnnotation(swift::Type, swift::DeclContext const*, swift::GenericSignature)","signatureAssert":"Assertion failed: (genericFuncType->getGenericSignature()->isEqual(genericSig) && \"if not, just use the GFT's signature instead below\"), function eraseArchetypes"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a<b> {
  c=d#^^#  macro d()
}
