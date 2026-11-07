// {"diagnosticStyle":"llvm","kind":"typecheck","signature":"swift::PrintingDiagnosticConsumer::printDiagnostic(swift::SourceManager&, swift::DiagnosticInfo const&)","signatureAssert":"Assertion failed: ((size_t)sys::locale::columnWidth(Fixit.getText()) == Fixit.getText().size()), function buildFixItLine","signatureNext":"PrintingDiagnosticConsumer::handleDiagnostic"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck -diagnostic-style=llvm %s
''
