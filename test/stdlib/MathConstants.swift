// RUN: %target-typecheck-verify-swift

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
  import Darwin
#elseif os(Linux) || os(FreeBSD) || os(OpenBSD) || os(PS4) || os(Android) || os(Cygwin) || os(Haiku) || os(WASI)
  import Glibc
#elseif os(Windows)
  import MSVCRT
#else
#error("Unsupported platform")
#endif

_ = M_PI // expected-warning {{is deprecated}}
_ = M_PI_2 // expected-warning {{is deprecated}}
_ = M_PI_4 // expected-warning {{is deprecated}}

_ = M_SQRT2 // expected-warning {{is deprecated}}
_ = M_SQRT1_2 // expected-warning {{is deprecated}}
