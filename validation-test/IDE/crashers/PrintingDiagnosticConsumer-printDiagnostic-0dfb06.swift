// {"kind":"complete","original":"002a4f4b","signature":"swift::PrintingDiagnosticConsumer::printDiagnostic(swift::SourceManager&, swift::DiagnosticInfo const&)","signatureAssert":"Assertion failed: ((size_t)sys::locale::columnWidth(Fixit.getText()) == Fixit.getText().size()), function buildFixItLine"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func b<c where #^^# == <#type#>>(a: <#type#>)
