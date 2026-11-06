// {"kind":"typecheck","original":"25ffa88b","signature":"formatDiagnosticArgument(llvm::StringRef, llvm::StringRef, llvm::ArrayRef<swift::DiagnosticArgument>, unsigned int, swift::DiagnosticFormatOptions, llvm::raw_ostream&)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"DiagnosticEngine::formatDiagnosticText"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a
  func b(@a  Double)
    b($c:
    .0
