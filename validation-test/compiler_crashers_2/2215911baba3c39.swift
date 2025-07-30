// {"kind":"emit-silgen","original":"e8914023","signature":"swift::Lowering::AbstractionPattern::getNumFunctionParams() const"}
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
