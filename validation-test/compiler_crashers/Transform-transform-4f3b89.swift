// {"kind":"emit-silgen","original":"a8fed261","signature":"(anonymous namespace)::Transform::transform(swift::Lowering::ManagedValue, swift::Lowering::AbstractionPattern, swift::CanType, swift::Lowering::AbstractionPattern, swift::CanType, swift::SILType, swift::Lowering::SGFContext)","signatureNext":"TranslateArguments::processSingle"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<b>(c: b) {
  let d: (inout (Int, String)) -> Int = \.0
}
