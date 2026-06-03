// {"kind":"typecheck","original":"5d9b1e60","signature":"swift::ProtocolConformanceRef llvm::function_ref<swift::ProtocolConformanceRef (swift::InFlightSubstitution&, swift::Type, swift::ProtocolDecl*)>::callback_fn<swift::constraints::Solution::computeSubstitutions(swift::NullablePtr<swift::ValueDecl>, swift::GenericSignature, swift::constraints::ConstraintLocator*) const::$_0>(long, swift::InFlightSubstitution&, swift::Type, swift::ProtocolDecl*)","signatureAssert":"Assertion failed: (!replacement->is<GenericTypeParamType>()), function operator()","signatureNext":"InFlightSubstitution::lookupConformance"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a<b> {
  c{enum d : Int{typealias RawValue = b case
  }
