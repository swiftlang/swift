// {"kind":"emit-silgen","original":"3c74143f","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILArgument const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifier"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@propertyWrapper
struct a<b> {
  var wrappedValue: b
  var projectedValue: b {
    get {
    }
    set {
    }
  }
}
func c<b>(@a d: b) {
}
