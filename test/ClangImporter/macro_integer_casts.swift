// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify

// The overflow diagnostics for imported macro constants are emitted during
// SILGen.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %s -verify -o /dev/null

// Failing for Android since #91256
// XFAIL: OS=linux-android, OS=linux-androideabi

// expected-warning@<unknown> * {{libc not found for }}

import macros

let _: CUnsignedInt = CAST_UNSIGNED_MINUS_ONE
let _: CUnsignedInt = CAST_UNSIGNED_MINUS_TEN
let _: TEST_DWORD = CAST_TYPEDEF_UNSIGNED_MINUS_ONE
let _: Int = CAST_SIZE_T_MINUS_ONE
let _: Int = CAST_NSUINTEGER_MINUS_ONE
