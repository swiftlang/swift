// {"kind":"typecheck","original":"b8ab3728","signature":"formatDiagnosticArgument(llvm::StringRef, llvm::StringRef, llvm::ArrayRef<swift::DiagnosticArgument>, unsigned int, swift::DiagnosticFormatOptions, llvm::raw_ostream&)"}
// RUN: not %target-swift-frontend -typecheck %s
func a(
  Double  -> String = {
    String(b: ""
    $0
