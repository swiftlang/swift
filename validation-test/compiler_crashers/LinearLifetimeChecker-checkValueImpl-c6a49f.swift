// {"kind":"emit-silgen","original":"776b50d4","signature":"swift::LinearLifetimeChecker::checkValueImpl(swift::SILValue, llvm::ArrayRef<swift::Operand*>, llvm::ArrayRef<swift::Operand*>, swift::LinearLifetimeChecker::ErrorBuilder&, std::__1::optional<llvm::function_ref<void (swift::SILBasicBlock*)>>, std::__1::optional<llvm::function_ref<void (swift::Operand*)>>)","signatureNext":"LinearLifetimeChecker::checkValue"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
enum a: Error {
  case b(String)
}
actor c {
  func d() throws(a) {
  }
}
func e() async throws {
  let f = c()
  do {
    try await f.d()
  } catch let error as a {
  }
}
