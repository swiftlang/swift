// {"kind":"emit-silgen","original":"2d904e3b","signature":"(anonymous namespace)::Transform::transform(swift::Lowering::ManagedValue, swift::Lowering::AbstractionPattern, swift::CanType, swift::Lowering::AbstractionPattern, swift::CanType, swift::SILType, swift::Lowering::SGFContext)","signatureNext":"Lowering::SILGenFunction::emitOrigToSubstValue"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<each b>(c: repeat inout each b) {
}
