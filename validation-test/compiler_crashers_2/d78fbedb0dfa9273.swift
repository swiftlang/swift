// {"kind":"typecheck","signature":"swift::TypeChecker::performTypoCorrection(swift::DeclContext*, swift::DeclRefKind, swift::Type, swift::optionset::OptionSet<swift::NameLookupFlags, unsigned int>, swift::TypoCorrectionResults&, swift::GenericSignature, unsigned int)","signatureAssert":"Assertion failed: (!baseTypeOrNull || !baseTypeOrNull->hasTypeParameter() || genericSig), function performTypoCorrection"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a<b {
  @propertyWrapper class c {wrappedValue:b var projectedValue: a? {
      @c var wrappedValue: b wrappedValue.d
