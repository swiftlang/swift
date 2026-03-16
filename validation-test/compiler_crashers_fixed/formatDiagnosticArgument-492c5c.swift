// {"kind":"typecheck","original":"e63729a4","signature":"formatDiagnosticArgument(llvm::StringRef, llvm::StringRef, llvm::ArrayRef<swift::DiagnosticArgument>, unsigned int, swift::DiagnosticFormatOptions, llvm::raw_ostream&)"}
// RUN: not %target-swift-frontend -typecheck %s
func a<b>(
  b  -> String = {
    "\($0)"
    func a(c: UInt64) {
      String(d: ""
      c
