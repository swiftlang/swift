// RUN: not --crash %target-typecheck-verify-swift -sdk %clang-importer-sdk -experimental-print-full-convention -use-clang-function-types

import ctypes

let _error : (@convention(c, cType: "(void *) (*)(void *)") (Int) -> Int)? = getFunctionPointer_()
