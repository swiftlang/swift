// {"kind":"typecheck","original":"07ee747b","signature":"swift::Type llvm::function_ref<swift::Type (swift::SubstitutableType*)>::callback_fn<swift::GenericEnvironment::mapElementTypeIntoPackContext(swift::Type) const::$_0>(long, swift::SubstitutableType*)","signatureAssert":"Assertion failed: (!elementEnv || elementEnv == archetype->getGenericEnvironment()), function operator()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>(c: (repeat each b) -> Void) -> (repeat each b) {
  { before, each in
    c(repeat before
    repeat each
