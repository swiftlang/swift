// {"kind":"emit-silgen","original":"7295ea39","signature":"swift::Lowering::TupleInitialization::copyOrInitValueInto(swift::Lowering::SILGenFunction&, swift::SILLocation, swift::Lowering::ManagedValue, bool)","signatureAssert":"Assertion failed: (value.isPlusOneOrTrivial(SGF) && \"Can not store a +0 value into memory?!\"), function copyOrInitValueInto","signatureNext":"Lowering::SILGenFunction::manageOpaqueValue"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<each b>(c: repeat (each b, each b)) {
  repeat (each c).0 ?? (each c).1
}
