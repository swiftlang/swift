// {"signature":"getDirectWriteAccessStrategy(swift::AbstractStorageDecl const*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(@_nonEphemeral UnsafeRawPointer) var arr { b a(&arr
