// {"kind":"emit-silgen","original":"caeb568b","signature":"(anonymous namespace)::RValueEmitter::visitAbstractClosureExpr(swift::AbstractClosureExpr*, swift::Lowering::SGFContext)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"Lowering::SILGenFunction::emitExprInto"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<b, each c>(d
  : repeat Optional<each c>, body
  : (repeat UnsafePointer<each c>) -> b) {
  body(repeat {
      each d
    })
}
