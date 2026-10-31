// RUN: %target-typecheck-verify-swift -swift-version 5 %s

@freestanding(codeItem)  // expected-cparser-error {{codeItem macros are an experimental feature that is not enabled}}
macro codeItems() // expected-error {{macro 'codeItems()' requires a definition}}
