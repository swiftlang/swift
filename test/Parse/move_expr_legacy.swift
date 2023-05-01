// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-move-only

// Temporary `_move` syntax is still parsed but raises a warning and fixit.
func oldMoveSyntax(x: String) {
  _ = _move x // expected-warning{{renamed to 'consume'}} {{7-12=consume}}
}
