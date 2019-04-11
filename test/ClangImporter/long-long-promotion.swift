// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s

import macros

func verifyIsSigned(_: Int64) { }
func verifyIsUnsigned(_: UInt64) { }

// Windows will not convert a long long value that overflows into an unsigned
// long long if it has the `ll` suffix of `i64` suffix.  Ensure that the type is
// imported appropriately.
#if os(Windows)
verifyIsSigned(LL_TO_ULL)
#else
verifyIsUnsigned(LL_TO_ULL)
#endif

