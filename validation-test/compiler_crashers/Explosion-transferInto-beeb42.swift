// {"kind":"emit-ir","signature":"swift::irgen::Explosion::transferInto(swift::irgen::Explosion&, unsigned int)","signatureAssert":"Assertion failed: (NextValue + n <= Values.size()), function claim","signatureNext":"emitApplyArgument"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
class b<c> {
  var d: c
  init(d: c) {
    self.d = d
  }
}
struct e: ~Copyable {
  deinit {}
}
actor c {
}
func f<g>(body: () throws -> g) rethrows -> g {
  try body()
}
func > (_ q: b<c>, _: Bool) -> c {
  let p = e()
  return f {
    p
    return q.d
  }
}
