// {"kind":"emit-silgen","original":"1ed76c3a","signature":"swift::Lowering::AbstractionPattern::initSwiftType(swift::SubstitutionMap, swift::CanGenericSignature, swift::CanType, swift::Lowering::AbstractionPattern::Kind)","signatureAssert":"Assertion failed: (OrigType == signature.getReducedType(origType)), function initSwiftType","signatureNext":"getSILFunctionType"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  associatedtype b
}
@available(SwiftStdlib 5.9, *)
struct c<each d> {
}
@available(SwiftStdlib 5.9, *)
actor e<each g: a> {
  var f = c<repeat (each g).b>()
}
