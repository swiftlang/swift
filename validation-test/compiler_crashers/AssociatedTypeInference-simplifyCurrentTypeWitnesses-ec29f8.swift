// {"kind":"typecheck","original":"aec05aef","signature":"swift::Type llvm::function_ref<swift::Type (swift::SubstitutableType*)>::callback_fn<(anonymous namespace)::AssociatedTypeInference::simplifyCurrentTypeWitnesses()::$_1::operator()(swift::TypeBase*) const::'lambda'(swift::SubstitutableType*)>(long, swift::SubstitutableType*)","signatureAssert":"Assertion failed: (type->isEqual(selfTy)), function operator()","signatureNext":"InFlightSubstitution::substType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  associatedtype c
  func d(b ) -> c
}
protocol e: a {
  associatedtype f
}
extension e {
    d(f )
  struct j<g {
    h {
    struct i : e {
    typealias f = j
