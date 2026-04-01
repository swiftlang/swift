// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s

import ctypes

// Don't crash or try to suggest editing an alias we can't access.
func takeSendable(_ fn: @Sendable fptr) { } // expected-error{{attribute '@Sendable' cannot be applied to a type alias}}
// expected-note@-1{{expand 'fptr' (aka '@convention(c) (Int32) -> Int32') to apply '@Sendable'}}{{35-39=@convention(c) (Int32) -> Int32}}
