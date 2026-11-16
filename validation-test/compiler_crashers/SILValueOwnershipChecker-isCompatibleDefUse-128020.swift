// {"kind":"emit-silgen","signature":"swift::SILValueOwnershipChecker::isCompatibleDefUse(swift::Operand*, swift::ValueOwnershipKind)","signatureNext":"SILValueOwnershipChecker::gatherNonGuaranteedUsers"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a(b: borrowing Int, c: Bool) -> Int {
  if c {
    return b
  } else {
    let d = b
    return d
  }
}
