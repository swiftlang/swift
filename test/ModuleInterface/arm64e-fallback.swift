// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -F %S/Inputs -module-cache-path %t -verify-ignore-unknown -Rmodule-interface-rebuild

// PtrAuthFramework only supports these OSes.
//
// REQUIRES: OS=tvos || OS=macosx || OS=ios
// UNSUPPORTED: DARWIN_SIMULATOR={{.*}}

// When run on arm64, this tests that we fall back to the arm64e interface, but
// build it with `#if _ptrauth(_arm64e)` off.
//
// When run on arm64e, this tests that we build the same interface with
// `#if _ptrauth(_arm64e)` on.
//
// REQUIRES: CPU=arm64 || CPU=arm64e

import PtrAuthFramework // expected-remark{{rebuilding module 'PtrAuthFramework' from interface}}

#if os(iOS)

#if _ptrauth(_arm64e)
public let x: Bool = iOSPtrAuth
#else
public let x: Bool = iOSNotPtrAuth
#endif

#elseif os(macOS)

#if _ptrauth(_arm64e)
public let x: Bool = macOSPtrAuth
#else
public let x: Bool = macOSNotPtrAuth
#endif

#else

#if _ptrauth(_arm64e)
public let x: Bool = tvOSPtrAuth
#else
public let x: Bool = tvOSNotPtrAuth
#endif

#endif
