// {"kind":"emit-silgen","original":"882569f1","signature":"swift::VarDecl::isLazilyInitializedGlobal() const","signatureAssert":"Assertion failed: (!getDeclContext()->isLocalContext() && \"not a global variable!\"), function isLazilyInitializedGlobal","signatureNext":"Lowering::SILGenFunction::emitGlobalVariableRef"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@available(SwiftStdlib 5.9, *)
struct a<each b> {
  var c: (repeat each b)
}
@available(SwiftStdlib 5.9, *)
func d<each b>(
  e: a<repeat each b>,
  g: repeat @escaping (each b) -> Bool
) -> () -> Bool {
  let f = e.c
  let i = (repeat each g)
  return {
    var h = true
    repeat h && (each i)(each f)
  }
}
