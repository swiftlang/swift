// RUN: %target-typecheck-verify-swift

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android)
import Glibc
#endif

_ = FLT_RADIX // expected-warning {{is deprecated}}

_ = FLT_MANT_DIG // expected-warning {{is deprecated}}
_ = FLT_MIN_EXP // expected-warning {{is deprecated}}
_ = FLT_MAX_EXP // expected-warning {{is deprecated}}
_ = FLT_MAX // expected-warning {{is deprecated}}
_ = FLT_EPSILON // expected-warning {{is deprecated}}
_ = FLT_MIN // expected-warning {{is deprecated}}
_ = FLT_TRUE_MIN // expected-warning {{is deprecated}}

_ = DBL_MANT_DIG // expected-warning {{is deprecated}}
_ = DBL_MIN_EXP // expected-warning {{is deprecated}}
_ = DBL_MAX_EXP // expected-warning {{is deprecated}}
_ = DBL_MAX // expected-warning {{is deprecated}}
_ = DBL_EPSILON // expected-warning {{is deprecated}}
_ = DBL_MIN // expected-warning {{is deprecated}}
_ = DBL_TRUE_MIN // expected-warning {{is deprecated}}
