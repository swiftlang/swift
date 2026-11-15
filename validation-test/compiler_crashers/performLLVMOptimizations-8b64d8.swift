// {"extraArgs":["-enable-experimental-feature","RawLayout"],"kind":"emit-ir","signature":"swift::performLLVMOptimizations(swift::IRGenOptions const&, swift::DiagnosticEngine&, llvm::sys::SmartMutex<false>*, llvm::Module*, llvm::TargetMachine*, llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem>, llvm::raw_pwrite_stream*)","signatureNext":"performLLVM"}
// RUN: not --crash %target-swift-frontend -emit-ir -enable-experimental-feature RawLayout %s
// REQUIRES: swift_feature_RawLayout
@available(SwiftStdlib 6.2, *)
struct a<b, let c: Int> {
}
@available(SwiftStdlib 6.2, *)
@_rawLayout(likeArrayOf: b, count: 3, movesAsLike)
struct d<b>: ~Copyable {
}
@available(SwiftStdlib 6.2, *)
struct e<b>: ~Copyable {
  var f: a<b, 8>
  var g: d<b>
}
