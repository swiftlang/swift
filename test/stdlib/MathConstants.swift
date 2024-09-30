// RUN: %target-typecheck-verify-swift -print-diagnostic-groups

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

_ = M_PI // expected-warning {{is deprecated: Please use 'Double.pi' or '.pi' to get the value of correct type and avoid casting. [DeprecatedDeclaration]}}
_ = M_PI_2 // expected-warning {{is deprecated: Please use 'Double.pi / 2' or '.pi / 2' to get the value of correct type and avoid casting. [DeprecatedDeclaration]}}
_ = M_PI_4 // expected-warning {{is deprecated: Please use 'Double.pi / 4' or '.pi / 4' to get the value of correct type and avoid casting. [DeprecatedDeclaration]}}

_ = M_SQRT2 // expected-warning {{is deprecated: Please use '2.squareRoot()'. [DeprecatedDeclaration]}}
_ = M_SQRT1_2 // expected-warning {{is deprecated: Please use '0.5.squareRoot()'. [DeprecatedDeclaration]}}
