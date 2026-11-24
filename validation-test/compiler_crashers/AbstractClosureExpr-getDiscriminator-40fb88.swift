// {"kind":"typecheck","signature":"swift::AbstractClosureExpr::getDiscriminator() const","signatureNext":"Verifier::verifyChecked"}
// RUN: not --crash %target-swift-frontend -typecheck -module-name M %s
protocol P {}
struct S: P {}
func h() -> some P { S() }
func f(_ a: Int?, _ b: Int) -> Int {
  func g() -> some P { S() }
  let x: @_opaqueReturnTypeOf("$s1M1hQryF", 0) __ = h()
  return a ?? b
}
