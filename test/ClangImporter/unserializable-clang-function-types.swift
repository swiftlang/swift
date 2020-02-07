// RUN: %target-swift-frontend -typecheck -swift-version 5 -emit-module-interface-path - -sdk %clang-importer-sdk -enable-library-evolution %s -experimental-print-full-convention -verify

import ctypes

public var f1 : UnserializableFunctionPointer?
// expected-error@-1 {{cannot export the underlying C type of the function type 'UnserializableFunctionPointer' (aka '@convention(c) () -> Optional<OpaquePointer>'); it may use anonymous types or types defined outside of a module}}
