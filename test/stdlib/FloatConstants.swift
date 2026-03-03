// RUN: %target-typecheck-verify-swift

#if canImport(Darwin)
  import Darwin
#elseif canImport(Glibc)
  import Glibc
#elseif os(WASI)
  import WASILibc
#elseif canImport(Android)
  import Android
#elseif os(Windows)
  import CRT
#else
#error("Unsupported platform")
#endif

_ = FLT_RADIX //  expected-warning {{is deprecated: Please use 'T.radix' to get the radix of a FloatingPoint type 'T'.}}{{documentation-file=deprecated-declaration}}

_ = FLT_MANT_DIG // expected-warning {{is deprecated: Please use 'Float.significandBitCount + 1'.}}{{documentation-file=deprecated-declaration}}
_ = FLT_MIN_EXP // expected-warning {{is deprecated: Please use 'Float.leastNormalMagnitude.exponent + 1'.}}{{documentation-file=deprecated-declaration}}
_ = FLT_MAX_EXP // expected-warning {{is deprecated: Please use 'Float.greatestFiniteMagnitude.exponent + 1'.}}{{documentation-file=deprecated-declaration}}
_ = FLT_MAX // expected-warning {{is deprecated: Please use 'Float.greatestFiniteMagnitude' or '.greatestFiniteMagnitude'.}}{{documentation-file=deprecated-declaration}}
_ = FLT_EPSILON // expected-warning {{is deprecated: Please use 'Float.ulpOfOne' or '.ulpOfOne'.}}{{documentation-file=deprecated-declaration}}
_ = FLT_MIN // expected-warning {{is deprecated: Please use 'Float.leastNormalMagnitude' or '.leastNormalMagnitude'.}}{{documentation-file=deprecated-declaration}}
_ = FLT_TRUE_MIN // expected-warning {{is deprecated: Please use 'Float.leastNonzeroMagnitude' or '.leastNonzeroMagnitude'.}}{{documentation-file=deprecated-declaration}}

_ = DBL_MANT_DIG // expected-warning {{is deprecated: Please use 'Double.significandBitCount + 1'.}}{{documentation-file=deprecated-declaration}}
_ = DBL_MIN_EXP // expected-warning {{is deprecated: Please use 'Double.leastNormalMagnitude.exponent + 1'.}}{{documentation-file=deprecated-declaration}}
_ = DBL_MAX_EXP // expected-warning {{is deprecated: Please use 'Double.greatestFiniteMagnitude.exponent + 1'.}}{{documentation-file=deprecated-declaration}}
_ = DBL_MAX // expected-warning {{is deprecated: Please use 'Double.greatestFiniteMagnitude' or '.greatestFiniteMagnitude'.}}{{documentation-file=deprecated-declaration}}
_ = DBL_EPSILON // expected-warning {{is deprecated: Please use 'Double.ulpOfOne' or '.ulpOfOne'.}}{{documentation-file=deprecated-declaration}}
_ = DBL_MIN // expected-warning {{is deprecated: Please use 'Double.leastNormalMagnitude' or '.leastNormalMagnitude'.}}{{documentation-file=deprecated-declaration}}
_ = DBL_TRUE_MIN // expected-warning {{is deprecated: Please use 'Double.leastNonzeroMagnitude' or '.leastNonzeroMagnitude'.}}{{documentation-file=deprecated-declaration}}
