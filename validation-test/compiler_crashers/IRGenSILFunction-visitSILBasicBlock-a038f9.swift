// {"kind":"emit-ir","original":"eeed16fc","signature":"(anonymous namespace)::IRGenSILFunction::visitSILBasicBlock(swift::SILBasicBlock*)","signatureAssert":"Assertion failed: (hasErrorResult()), function getMutableErrorResult","signatureNext":"IRGenSILFunction::emitSILFunction"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
protocol a {
  associatedtype b: c where b.d == Self
  associatedtype e: Collection where e.Element == Self
  func o(f: b) -> e
}
protocol c {
  associatedtype d: a where d.b == Self
  associatedtype n
  func o(g: d, h: n) -> Bool
}
struct i: a {
  func o(f: j) -> [i] {
    []
  }
}
struct j: c {
  func o(g: i, h: String) -> Bool {
    true
  }
}
protocol k {
  associatedtype l
}
extension k {
  func m() throws(l) where l == Never {
  }
}
