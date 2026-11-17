// {"kind":"emit-ir","original":"b54cee14","signature":"(anonymous namespace)::IRGenSILFunction::visitBeginAccessInst(swift::BeginAccessInst*)","signatureNext":"IRGenSILFunction::emitSILFunction"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
struct a {
  var b: Int
}
struct c {
  var d: Int
  consuming func e() {
  }
}
func f(
  l: inout a,
  g: c
) -> () -> Void {
  struct h {
    let i: Int
    func m(
      _: inout a, _ j: consuming c
    ) -> () -> Void {
      let k = {
        j.e()
      }
      return withoutActuallyEscaping(k) {
        $0
      }
    }
  }
  let n = h(i: l.b)
  return n.m(&l, g)
}
