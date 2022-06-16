// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -sdk %clang-importer-sdk -experimental-print-full-convention -use-clang-function-types
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -sdk %clang-importer-sdk
// RUN: %FileCheck %s < %t.swiftinterface

import ctypes

// CHECK: f1: (@convention(c, cType: "size_t (*)(size_t)") (Swift.Int) -> Swift.Int)?
public let f1 = getFunctionPointer_()

// CHECK: f2: (@convention(c) ((@convention(c, cType: "size_t (*)(size_t)") (Swift.Int) -> Swift.Int)?) -> (@convention(c, cType: "size_t (*)(size_t)") (Swift.Int) -> Swift.Int)?)?
public let f2 = getHigherOrderFunctionPointer()

// CHECK: f3: () -> (@convention(c) (Swift.UnsafeMutablePointer<ctypes.Dummy>?) -> Swift.UnsafeMutablePointer<ctypes.Dummy>?)?
public let f3 = getFunctionPointer3
