// {"kind":"emit-ir","original":"090d151a","signature":"swift::irgen::emitWitnessTableRef(swift::irgen::IRGenFunction&, swift::CanType, llvm::Value**, swift::ProtocolConformanceRef)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"forEachProtocolWitnessTable"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
protocol a {
}
struct b: a {
}
func c() -> some a {
  b()
}
func d() -> a.Type {
  let e = c()
  return type(of: e)
}
