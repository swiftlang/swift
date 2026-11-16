// {"kind":"typecheck","signature":"swift::AbstractClosureExpr::getDiscriminator() const","signatureNext":"Verifier::verifyChecked"}
// RUN: not --crash %target-swift-frontend -typecheck -disable-objc-attr-requires-foundation-module %s
// REQUIRES: objc_interop
func f(_ a: Int?, _ b: Int) -> Int {
  @objc class Local {}
  let g: @convention(block) (Local) -> Void = { _ in }
  _ = g
  return a ?? b
}
