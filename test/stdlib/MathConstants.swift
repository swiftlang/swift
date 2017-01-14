// RUN: %target-typecheck-verify-swift

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android)
import Glibc
#endif

_ = M_PI // expected-warning {{is deprecated}}
_ = M_PI_2 // expected-warning {{is deprecated}}
_ = M_PI_4 // expected-warning {{is deprecated}}

_ = M_SQRT2 // expected-warning {{is deprecated}}
_ = M_SQRT1_2 // expected-warning {{is deprecated}}
