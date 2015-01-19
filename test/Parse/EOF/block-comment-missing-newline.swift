// RUN: %target-parse-verify-swift

// This file does not end in a trailing newline; this is deliberate!
/* unterminated block comment  expected-note{{comment started here}} expected-error{{unterminated '/*' comment}}{{127-127=*/}}