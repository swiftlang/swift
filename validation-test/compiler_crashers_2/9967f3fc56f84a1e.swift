// {"signature":"formatDiagnosticArgument(llvm::StringRef, llvm::StringRef, llvm::ArrayRef<swift::DiagnosticArgument>, unsigned int, swift::DiagnosticFormatOptions, llvm::raw_ostream&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a func b(@a Int) b($c:d
