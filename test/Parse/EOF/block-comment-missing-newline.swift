// RUN: %swift %s -verify

// This file does not end in a trailing newline; this is deliberate!
/* unterminated block comment expected-warning{{missing newline at end of file}} expected-note{{comment started here}} expected-error{{unterminated '/*' comment}} {{178-178=*/}}