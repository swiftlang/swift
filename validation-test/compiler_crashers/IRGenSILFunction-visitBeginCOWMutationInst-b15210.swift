// {"extraArgs":["-enable-experimental-feature","BuiltinModule"],"kind":"emit-ir","signature":"(anonymous namespace)::IRGenSILFunction::visitBeginCOWMutationInst(swift::BeginCOWMutationInst*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"IRGenSILFunction::emitSILFunction"}
// RUN: not --crash %target-swift-frontend -emit-ir -enable-experimental-feature BuiltinModule %s
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_BuiltinModule
import Builtin
class a<b> {
}
class c<t> {
}
func d<b, e>(
  f: a<b>.Type,
  v: Int,
  g: e.Type
) -> (a<b>, Builtin.RawPointer, c<b>, Builtin.RawPointer) {
  let h = v._builtinWordValue
  let i = Builtin.allocWithTailElems_1(f, h, g)
  let j = Builtin.projectTailElems(i, g)
  let k = Builtin.allocWithTailElems_1(c<b>.self, h, Int8.self)
  let l = Builtin.projectTailElems(k, Int8.self)
  return (i, j, k, l)
}
struct u<m: AnyObject> {
  var n: m?
  mutating func o() -> (Bool) {
    Bool(Builtin.beginCOWMutation(&n))
  }
}
