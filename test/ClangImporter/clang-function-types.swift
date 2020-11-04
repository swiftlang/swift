// RUN: %target-swift-frontend -typecheck -swift-version 5 -emit-module-interface-path - -sdk %clang-importer-sdk -enable-library-evolution %s -experimental-print-full-convention -use-clang-function-types | %FileCheck %s

import ctypes

// CHECK: f1: (@convention(c, cType: "size_t (*)(size_t)") (Swift.Int) -> Swift.Int)?
public let f1 = getFunctionPointer_()

// CHECK: f2: (@convention(c) ((@convention(c, cType: "size_t (*)(size_t)") (Swift.Int) -> Swift.Int)?) -> (@convention(c, cType: "size_t (*)(size_t)") (Swift.Int) -> Swift.Int)?)?
public let f2 = getHigherOrderFunctionPointer()

// CHECK: f3: () -> (@convention(c) (Swift.UnsafeMutablePointer<ctypes.Dummy>?) -> Swift.UnsafeMutablePointer<ctypes.Dummy>?)?
public let f3 = getFunctionPointer3
