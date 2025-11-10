// {"kind":"complete","original":"5070984e","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: ((range.isValid()) && \"scope has invalid source range\"), function getCharSourceRangeOfScope"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@a var b {
  @#^^#< #declaration
  #>
}
