// RUN: %target-swift-frontend %s -emit-sil -verify

// assertionFailure() is now @_transparent (previously @inlinable), so the
// compiler can see that it never returns, eliminating the "missing return" error.
func assertionFailure_isNoreturn() -> Int {
  _ = 0
  assertionFailure("")
  // No error expected - assertionFailure() is recognized as Never-returning
}

