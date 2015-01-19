// RUN: %target-parse-verify-swift

// Parser used to crash while parsing a file where the last token was an
// interpolated string literal.

"\(1)"
