// {"kind":"typecheck","signature":"swift::Type llvm::function_ref<swift::Type (swift::SubstitutableType*)>::callback_fn<swift::GenericEnvironment::mapElementTypeIntoPackContext(swift::Type) const::$_0>(long, swift::SubstitutableType*)","signatureAssert":"Assertion failed: (!elementEnv || elementEnv == archetype->getGenericEnvironment()), function operator()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b, each c>(d: repeat each b e: repeat each c) {
  for f  repeat each e {
    repeat (d
    f
