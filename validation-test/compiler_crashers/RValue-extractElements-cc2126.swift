// {"kind":"emit-silgen","original":"0c17ae00","signature":"swift::Lowering::RValue::extractElements(llvm::SmallVectorImpl<swift::Lowering::RValue>&) &&","signatureAssert":"Assertion failed: (!tupleTy.containsPackExpansionType() && \"can't extract elements from tuples containing pack expansions \" \"right now\"), function extractElements","signatureNext":"Lowering::ArgumentSourceExpansion::ArgumentSourceExpansion"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@available(SwiftStdlib 5.9, *)
struct a<each b> {
  let c: String
  let d: (repeat each b)
  func e() {
    a(c: c, d: d)
  }
}
