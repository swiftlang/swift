// {"kind":"complete","original":"f53cb65c","signature":"(anonymous namespace)::SetLocalDiscriminators::setLocalDiscriminator(swift::ValueDecl*)","signatureAssert":"Assertion failed: (LocalDiscriminator == InvalidDiscriminator && \"LocalDiscriminator is set multiple times\"), function setLocalDiscriminator"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{ $a in
  {
    enum b {
    c -> b {
      #^^#
