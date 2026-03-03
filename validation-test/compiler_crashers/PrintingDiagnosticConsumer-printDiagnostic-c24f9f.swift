// {"diagnosticStyle":"llvm","kind":"typecheck","signature":"swift::PrintingDiagnosticConsumer::printDiagnostic(swift::SourceManager&, swift::DiagnosticInfo const&)","signatureAssert":"Assertion failed: ((size_t)sys::locale::columnWidth(Fixit.getText()) == Fixit.getText().size()), function buildFixItLine"}
// RUN: not --crash %target-swift-frontend -typecheck -diagnostic-style=llvm %s
''
