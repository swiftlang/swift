// {"signature":"swift::constraints::NonEphemeralConversionFailure::emitSuggestionNotes() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(@_nonEphemeral UnsafePointer<Int8>?? ) var b = "" a(b
