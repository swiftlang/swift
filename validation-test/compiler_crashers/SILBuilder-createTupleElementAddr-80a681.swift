// {"kind":"emit-silgen","signature":"swift::SILBuilder::createTupleElementAddr(swift::SILLocation, swift::SILValue, unsigned int, swift::SILType)","signatureAssert":"Assertion failed: (!Operand->getType().castTo<TupleType>().containsPackExpansionType() && \"tuples with pack expansions must be indexed with \" \"tuple_pack_element_addr\"), function createTupleElementAddr","signatureNext":"TupleElementComponent::project"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@available(SwiftStdlib 6.0, *)
struct a<each b> {
  var c: (repeat each b)
  init(_: repeat each b) {
  }
}
@available(SwiftStdlib 6.0, *)
func d<each b>(e: repeat each b) {
  var g = a(0, repeat each e)
  let f: (Int, repeat each b) = (g.c.0, repeat each e)
}
