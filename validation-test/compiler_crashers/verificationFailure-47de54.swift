// {"kind":"emit-silgen","original":"8ac1a888","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifier::checkAssignByWrapperArgsRecursively"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@propertyWrapper
struct a<b> {
  var wrappedValue: b
}
@available(SwiftStdlib 5.9, *)
class c<each b> {
  @a var d: (repeat each b)
  init(e: repeat each b) {
    d = (repeat each e)
  }
}
