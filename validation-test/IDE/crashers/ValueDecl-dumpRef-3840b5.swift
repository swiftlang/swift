// {"kind":"complete","original":"c2850df8","signature":"swift::ValueDecl::dumpRef(llvm::raw_ostream&) const","signatureAssert":"Assertion failed: (ext->canNeverBeBound() && \"Should have been bound by bindExtensions\"), function evaluate"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@abi(
  extension { # a(#^^#<#declaration#>)
  <#declaration#>
