// RUN: %target-typecheck-verify-swift

// This file does not end in a trailing newline; this is deliberate!
/* unterminated block comment expected-cparser-note{{comment started here}} expected-astgen-error{{unterminated}} expected-cparser-error{{unterminated '/*' comment}}{{180-180=*/}}