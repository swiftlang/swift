// {"extraArgs":["-enable-experimental-feature","BuiltinModule"],"kind":"emit-ir","signature":"swift::irgen::IRBuilder::CreateElementBitCast(swift::irgen::Address, llvm::Type*, llvm::Twine const&)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"EnumPayload::load"}
// RUN: not --crash %target-swift-frontend -emit-ir -enable-experimental-feature BuiltinModule %s
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_BuiltinModule
import Builtin
enum a {
  case b(Double)
  case c(Int)
}
func d(e: a) {
  Builtin.getEnumTag(e)
}
