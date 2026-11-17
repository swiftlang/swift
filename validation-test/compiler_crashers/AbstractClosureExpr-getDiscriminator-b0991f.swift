// {"kind":"typecheck","signature":"swift::AbstractClosureExpr::getDiscriminator() const","signatureNext":"Verifier::verifyChecked"}
// RUN: not --crash %target-swift-frontend -typecheck -disable-objc-attr-requires-foundation-module %s
// REQUIRES: objc_interop
func f(_ a: Int?, _ b: Int) -> Int {
  class Local {
    @objc(copyWithZone:) func copy(_ zone: OpaquePointer?) {}
  }
  return a ?? b
}
