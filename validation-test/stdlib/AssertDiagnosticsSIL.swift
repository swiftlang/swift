// RUN: %target-swift-frontend %s -emit-sil -verify

func assertionFailure_isNotNoreturn() -> Int {
  assertionFailure("")
} // expected-error {{missing return in a function expected to return 'Int'}}

