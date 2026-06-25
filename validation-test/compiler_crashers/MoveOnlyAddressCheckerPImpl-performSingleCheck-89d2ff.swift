// {"kind":"emit-sil","original":"93a23670","signature":"(anonymous namespace)::MoveOnlyAddressCheckerPImpl::performSingleCheck(swift::MarkUnresolvedNonCopyableValueInst*)","signatureAssert":"Assertion failed: (hadAnyErrorUsers == emittedDiagnostic), function compute","signatureNext":"MoveOnlyAddressChecker::check"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
@available(SwiftStdlib 6.2, *)
enum a<let b: Int>: ~Copyable {
  case c
  case d(InlineArray<b, Int32>)
  consuming func e() {
  }
}
@available(SwiftStdlib 6.2, *)
struct f<let b: Int, let g: Int> {
  func j() {
    for h in 0..<g {
      var i: a<b> = .c
      i.e()
    }
  }
}
