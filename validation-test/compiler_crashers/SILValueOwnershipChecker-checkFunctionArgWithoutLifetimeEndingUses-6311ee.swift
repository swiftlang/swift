// {"extraArgs":["-O"],"kind":"emit-sil","signature":"swift::SILValueOwnershipChecker::checkFunctionArgWithoutLifetimeEndingUses(swift::SILFunctionArgument*, llvm::ArrayRef<swift::Operand*>, llvm::ArrayRef<swift::Operand*>)","signatureNext":"SILValueOwnershipChecker::checkValueWithoutLifetimeEndingUses"}
// RUN: not --crash %target-swift-frontend -emit-sil -O %s
public
  enum a: ~Copyable
{
  case b
  case c
}
public func d(e: consuming a, f: Int) -> Int {
  switch consume e {
  case .b:
    break
  case .c:
    break
  }
  return f
}
