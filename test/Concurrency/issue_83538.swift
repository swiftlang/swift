// RUN: %target-typecheck-verify-swift -disable-availability-checking

protocol P {}
struct S: isolated P {} // expected-error {{'isolated' may only be used on parameters}}
