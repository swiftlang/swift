// {"kind":"emit-silgen","original":"2e6803f2","signature":"(anonymous namespace)::ArgEmitter::emitPackArg(llvm::MutableArrayRef<swift::Lowering::ArgumentSource>, swift::Lowering::AbstractionPattern)","signatureAssert":"Assertion failed: (param.isPack() && \"emitting pack argument into non-pack parameter\"), function emitPackArg","signatureNext":"ArgEmitter::emitPreparedArgs"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@available(SwiftStdlib 5.9, *)
struct b<each a> {
  subscript<each c>(h: repeat each c) -> () {
  }
}
@available(SwiftStdlib 5.9, *)
func d<each e, each c>(g: b<repeat each e>, f: repeat each c) {
  g[repeat each f]
}
