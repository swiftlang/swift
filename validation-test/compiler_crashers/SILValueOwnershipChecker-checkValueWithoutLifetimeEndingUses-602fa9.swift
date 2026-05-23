// {"kind":"emit-silgen","original":"4b0093a4","signature":"swift::SILValueOwnershipChecker::checkValueWithoutLifetimeEndingUses(llvm::ArrayRef<swift::Operand*>, llvm::ArrayRef<swift::Operand*>)","signatureNext":"SILValueOwnershipChecker::checkUses"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
class a {
}
protocol b {
  associatedtype c
  consuming func d(e: c) async
}
actor f: b {
  func d(e: a) {
  }
}
