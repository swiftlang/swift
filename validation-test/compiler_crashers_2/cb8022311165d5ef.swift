// {"kind":"typecheck","signature":"swift::AbstractStorageDecl::getAccessStrategy(swift::AccessSemantics, swift::AccessKind, swift::ModuleDecl*, swift::ResilienceExpansion, std::__1::optional<std::__1::pair<swift::SourceRange, swift::DeclContext const*>>, bool) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(@_nonEphemeral UnsafeRawPointer) var arr { b a(&arr
