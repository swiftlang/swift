// RUN: %target-typecheck-verify-swift -DFLAG=42

#flagValue("FLAG") // expected-warning {{integer literal is unused}}

_ = #flagValue(99) // expected-error {{#flagValue argument must be string literal}}
// expected-error@-1 {{'nil' requires a contextual type}}

_ = #flagValue("FLAG2")
// expected-error@-1 {{#flagValue no value found for key 'FLAG2' and no default}}
// expected-error@-2 {{'nil' requires a contextual type}}

// Custom compilation with comparisons

#if FLAG == "42"
UNDEF // expected-error {{use of unresolved identifier 'UNDEF'}}
#endif

#if FLAG == 42.0
UNDEF // expected-error {{use of unresolved identifier 'UNDEF'}}
#endif

#if FLAG <= 42.0
UNDEF // expected-error {{use of unresolved identifier 'UNDEF'}}
#endif

#if 42.0 // expected-error {{invalid context for literal in conditional}}
UNDEF
#endif

#if FLAG && !FLAG2
UNDEF // expected-error {{use of unresolved identifier 'UNDEF'}}
#endif

#if FLAG2
UNDEF
#endif
