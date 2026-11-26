// {"extraArgs":["-language-mode","6"],"kind":"emit-sil","signature":"swift::AccessUseDefChainVisitor<(anonymous namespace)::AddressBaseComputingVisitor, swift::SILValue>::visit(swift::SILValue)","signatureNext":"RegionAnalysisValueMap::getUnderlyingTrackedValueHelperAddress"}
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name Lib -o %t/Lib.swiftmodule %s
// RUN: not --crash %target-swift-frontend -emit-sil -language-mode 6 %t/Lib.swiftmodule
@inlinable public func f(_ a: UnsafePointer<UnsafeRawPointer>) {
  var a = a
  while true { a = a.pointee.assumingMemoryBound(to: UnsafeRawPointer.self) }
}
