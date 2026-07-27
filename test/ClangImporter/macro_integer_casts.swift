// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify

import macros

let _: CUnsignedInt = CAST_UNSIGNED_MINUS_ONE
let _: CUnsignedInt = CAST_UNSIGNED_MINUS_TEN
let _: TEST_DWORD = CAST_TYPEDEF_UNSIGNED_MINUS_ONE
