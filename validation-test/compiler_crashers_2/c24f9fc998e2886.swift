// {"signature":"swift::PrintingDiagnosticConsumer::printDiagnostic(swift::SourceManager&, swift::DiagnosticInfo const&)"}
// RUN: not --crash %target-swift-frontend -typecheck -diagnostic-style=llvm %s
''
