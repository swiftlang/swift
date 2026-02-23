// {"kind":"typecheck","original":"4f0b1241","signature":"swift::Type llvm::function_ref<swift::Type (swift::SubstitutableType*)>::callback_fn<(anonymous namespace)::BuildForwardingSubstitutions>(long, swift::SubstitutableType*)","signatureAssert":"Assertion failed: (rootParameterPacks.size() >= 1), function getSingletonPackExpansion"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a: b
  struct b<each c: a {  c:
