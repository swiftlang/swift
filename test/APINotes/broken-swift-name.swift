// RUN: %target-typecheck-verify-swift -I %S/Inputs/broken-modules
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

  // Ensure that definitions with invalid API notes are still available
  // under their original names.
  ZXSpectrumGetRegister(&x, 0)
  ZXSpectrumSetRegister(&x, 0, 1)
  ZXSpectrumGetMisnamedRegister(&x, 0)
  // TODO: Conservative validation in Clang doesn't reject the API name here
  // ZXSpectrumSetMisnamedRegister(&x, 0, 1)

  ZXSpectrumHelperReset()
}
