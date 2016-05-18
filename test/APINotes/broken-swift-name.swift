// RUN: %target-parse-verify-swift -I %S/Inputs/broken-modules
import BrokenAPINotes

func testBrokenSwiftName(x: inout ZXSpectrum) {
  _ = x.accumulator
  x.accumulator = 0

  // The functions that import as `register`'s accessors have incorrect
  // signatures for a getter or setter. Ensure we drop the import instead of
  // forming an invalid property.
  _ = x.register // expected-error{{has no member}}
  x.register = 0 // expected-error{{has no member}}

  _ = x.misnamedRegister // expected-error{{has no member}}
  x.misnamedRegister = 0 // expected-error{{has no member}}
}
