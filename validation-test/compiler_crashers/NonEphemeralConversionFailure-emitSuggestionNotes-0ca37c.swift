// {"kind":"typecheck","signature":"swift::constraints::NonEphemeralConversionFailure::emitSuggestionNotes() const","signatureAssert":"Assertion failed: (pointeeType && \"Expected a pointer!\"), function operator()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(@_nonEphemeral UnsafePointer<Int8>?? ) var b = "" a(b
