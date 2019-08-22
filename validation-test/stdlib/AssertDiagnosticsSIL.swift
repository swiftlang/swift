// RUN: %target-swift-frontend %s -emit-sil -verify

func assertionFailure_isNotNoreturn() -> Int {
  _ = 0 // Don't implicitly return the assertionFailure call.
  assertionFailure("")
} // expected-error {{missing return in a function expected to return 'Int'}}

