// {"kind":"typecheck","signature":"swift::AbstractStorageDecl::getAccessStrategy(swift::AccessSemantics, swift::AccessKind, swift::ModuleDecl*, swift::ResilienceExpansion, std::__1::optional<std::__1::pair<swift::SourceRange, swift::DeclContext const*>>, bool) const","signatureAssert":"Assertion failed: (isa<VarDecl>(storage) && cast<VarDecl>(storage)->isLet() && \"mutation of a immutable variable that isn't a let\"), function getDirectReadWriteAccessStrategy"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(@_nonEphemeral UnsafeRawPointer) var arr { b a(&arr
