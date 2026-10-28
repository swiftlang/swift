// {"kind":"typecheck","signature":"swift::diagnoseMissingOwnership(swift::ParamSpecifier, swift::TypeRepr*, swift::Type, swift::TypeResolution const&)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"validateParameterType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a
  typealias b<c> = a
  extension b<Int> {
      d(e: b
