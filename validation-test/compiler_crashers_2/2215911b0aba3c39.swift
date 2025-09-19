// {"kind":"emit-silgen","original":"e8914023","signature":"swift::Lowering::AbstractionPattern::getNumFunctionParams() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<b, each c>(
  _: (repeat each c)!,
  _: b
) {
}
struct d {
  let e = ""
  static func == (g: Self, f: Self) {
    let h =
      \.e as (Self) -> _
    a(h, f)
  }
}
