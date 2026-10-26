// {"kind":"merge-modules","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILFunction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifier::visitSILFunction"}
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -cxx-interoperability-mode=default -I %S/Inputs/verificationFailure-aa2f3b %s -module-name Lib -o %t/Lib.swiftmodule
// RUN: not --crash %target-swift-frontend -merge-modules -emit-module -parse-as-library -cxx-interoperability-mode=default -I %S/Inputs/verificationFailure-aa2f3b %t/Lib.swiftmodule -module-name Lib -o %t/Lib2.swiftmodule

import SubpartVec

@inlinable public func sum(_ v: SubpartVec) -> Int32 {
  var s: Int32 = 0
  for p in v { s += p.x }
  return s
}
