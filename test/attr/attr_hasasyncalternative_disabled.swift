// REQUIRES: concurrency

// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

@hasAsyncAlternative // expected-error {{'hasAsyncAlternative' support is required to be explicitly enabled using -experimental-has-async-alternative-attribute}}
func func1() {}
