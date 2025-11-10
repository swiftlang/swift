// {"kind":"emit-sil","original":"af5c6219","signature":"(anonymous namespace)::AvailabilitySet::getConditional(unsigned int) const","signatureAssert":"Assertion failed: (Idx < size() && \"Out-of-bounds Bit access.\"), function operator[]"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
struct a<b> {
  let c: String
  let d: b
}
extension a where b == Void {
  init() {
    d <= ()
  }
}
