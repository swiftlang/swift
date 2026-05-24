// {"kind":"emit-ir","original":"669b419a","signature":"swift::irgen::emitScalarExistentialDowncast(swift::irgen::IRGenFunction&, llvm::Value*, swift::SILType, swift::SILType, swift::irgen::CheckedCastMode, bool, std::__1::optional<swift::MetatypeRepresentation>, swift::irgen::Explosion&)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"emitScalarCheckedCast"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
protocol a {
}
func b<c, d>(e: c, f: d) -> Bool {
  d.self
    is AnyClass.Type
  let g = a.self
  let h = a.self
  let i = g == h
  return i
}
