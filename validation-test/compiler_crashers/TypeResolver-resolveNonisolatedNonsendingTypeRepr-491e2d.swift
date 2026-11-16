// {"kind":"typecheck","signature":"(anonymous namespace)::TypeResolver::resolveNonisolatedNonsendingTypeRepr(swift::NonisolatedNonsendingTypeRepr*, swift::TypeResolutionOptions)","signatureNext":"TypeResolver::resolveType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a()
: nonisolated(nonsending)(nonisolated(nonsending)() async->Void
