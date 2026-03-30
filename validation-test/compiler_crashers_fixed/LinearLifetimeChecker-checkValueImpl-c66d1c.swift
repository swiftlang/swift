// {"kind":"emit-silgen","original":"5d49df12","signature":"swift::LinearLifetimeChecker::checkValueImpl(swift::SILValue, llvm::ArrayRef<swift::Operand*>, llvm::ArrayRef<swift::Operand*>, swift::LinearLifetimeChecker::ErrorBuilder&, std::__1::optional<llvm::function_ref<void (swift::SILBasicBlock*)>>, std::__1::optional<llvm::function_ref<void (swift::Operand*)>>)","signatureNext":"LinearLifetimeChecker::checkValue"}
// RUN: %target-swift-frontend -emit-silgen %s > /dev/null
class a {
  var b: c
  init(_: [UInt8]) {
  }
}
struct c {
  var bytes: [UInt8]
}
func d() {
  let e = a([])
  _ = e.b.bytes
}
