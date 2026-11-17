// {"kind":"emit-silgen","original":"3e91397d","signature":"swift::Lowering::SILGenFunction::manageOpaqueValue(swift::Lowering::ManagedValue, swift::SILLocation, swift::Lowering::SGFContext)","signatureAssert":"Assertion failed: (isInit && \"Only initialization is supported for refutable patterns\"), function copyOrInitValueIntoImpl","signatureNext":"RValueEmitter::visitPackElementExpr"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@available(SwiftStdlib 6.0, *)
struct a<each b> {
  var c:
    (
      repeat (each b)?
    )
  {
    let d = c
    (repeat { e in
      if e, let f = each d {
      }
    })
  }
}
