// {"kind":"complete","original":"5d283fee","signature":"swift::TypeRepr::print(swift::ASTPrinter&, swift::PrintOptions const&, swift::optionset::OptionSet<swift::NonRecursivePrintOption, unsigned int>) const","signatureAssert":"Assertion failed: (ext->canNeverBeBound() && \"Should have been bound by bindExtensions\"), function evaluate"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@abi(
  extension { #^^#<#declaration#>)
  <#declaration#>
