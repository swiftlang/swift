// {"kind":"typecheck","original":"30ab265f","signature":"formatDiagnosticArgument(llvm::StringRef, llvm::StringRef, llvm::ArrayRef<swift::DiagnosticArgument>, unsigned int, swift::DiagnosticFormatOptions, llvm::raw_ostream&)","signatureAssert":"Assertion failed: ((!ModifierArguments.empty() || foundPipe) && \"Index beyond bounds in %select modifier\"), function formatSelectionArgument"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
// REQUIRES: objc_interop
import Foundation
@objc protocol a {
  optional var name: String
}
extension NSObject: a {
  var names = <#expression#>
}
