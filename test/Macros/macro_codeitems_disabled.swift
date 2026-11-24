// RUN: %target-typecheck-verify-swift %s

@freestanding(codeItem)  // expected-error {{codeItem macros are an experimental feature that is not enabled}}
macro codeItems() // expected-error {{macro 'codeItems()' requires a definition}}
