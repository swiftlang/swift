// {"kind":"emit-silgen","original":"9be99b8e","signature":"swift::Lowering::AbstractionPattern::getTupleElementType(unsigned int) const","signatureNext":"Transform::transform"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@available(SwiftStdlib 5.9, *)
struct a<each b> {
  let c: (repeat each b)
}
@available(SwiftStdlib 5.9, *)
func d() {
  a(c: (Int.self, Float.self)).c
}
