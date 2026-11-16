// {"kind":"emit-ir","signature":"(anonymous namespace)::RewriteUser::rewrite()","signatureAssert":"Assertion failed: (srcAddr->getType() == destAddr->getType()), function createCopyAddr","signatureNext":"runPeepholesAndReg2Mem"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
struct h {
  var a, b, d, e, f, g: Int
}
struct i {
  var j: ((h) -> h, h)
  var o: h
  var k: (h) -> h
  var l: ((h) -> h, h)? {
    get {
      j
    }
    set {
    }
  }
}
func m(c: inout i) -> Int {
  c.l = (c.k, c.o)
  let n = c.o
  return n.b
}
