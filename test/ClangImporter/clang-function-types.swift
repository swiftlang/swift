// RUN: %target-swift-frontend -typecheck -swift-version 5 -emit-module-interface-path - -sdk %clang-importer-sdk -enable-library-evolution %s -experimental-print-full-convention | %FileCheck %s

import ctypes

// CHECK: f1: (@convention(c, cType: "int (*)(int)") (Swift.Int32) -> Swift.Int32)?
public let f1 = getFunctionPointer_()

// CHECK: f2: (@convention(c, cType: "int (*(*)(int (*)(int)))(int)") ((@convention(c, cType: "int (*)(int)") (Swift.Int32) -> Swift.Int32)?) -> (@convention(c, cType: "int (*)(int)") (Swift.Int32) -> Swift.Int32)?)?
public let f2 = getHigherOrderFunctionPointer()

// CHECK: f3: () -> (@convention(c, cType: "Dummy *(*)(Dummy *)") (Swift.UnsafeMutablePointer<ctypes.Dummy>?) -> Swift.UnsafeMutablePointer<ctypes.Dummy>?)?
public let f3 = getFunctionPointer3
