//
// Build objc_implementation.framework
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/frameworks)
// RUN: %empty-directory(%t/frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule)
// RUN: %empty-directory(%t/frameworks/objc_implementation.framework/Headers)
// RUN: cp %S/Inputs/objc_implementation.modulemap %t/frameworks/objc_implementation.framework/Modules/module.modulemap
// RUN: cp %S/Inputs/objc_implementation.h %t/frameworks/objc_implementation.framework/Headers
// RUN: %target-build-swift-dylib(%t/frameworks/objc_implementation.framework/objc_implementation) -emit-module-path %t/frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule/%module-target-triple.swiftmodule -module-name objc_implementation -F %t/frameworks -import-underlying-module -Xlinker -install_name -Xlinker %t/frameworks/objc_implementation.framework/objc_implementation %S/objc_implementation.swift

//
// Execute this file
//
// RUN: %empty-directory(%t/swiftmod)
// RUN: %target-build-swift %s -module-cache-path %t/swiftmod/mcp -F %t/frameworks -o %t/swiftmod/a.out -module-name main
// RUN: %target-codesign %t/swiftmod/a.out
// RUN: %target-run %t/swiftmod/a.out | %FileCheck %s

//
// Execute again, without the swiftmodule this time
//
// RUN: mv %t/frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule %t/frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule.disabled
// RUN: %empty-directory(%t/clangmod)
// RUN: %target-build-swift %s -module-cache-path %t/clangmod/mcp -F %t/frameworks -o %t/clangmod/a.out -module-name main
// RUN: %target-codesign %t/clangmod/a.out
// RUN: %target-run %t/clangmod/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import objc_implementation

ImplClass.runTests()
// CHECK: ImplClass.someMethod()
// CHECK: SwiftSubclass.someMethod()

print(ImplClass().someMethod())
// CHECK: ImplClass.someMethod()

class SwiftClientSubclass: ImplClass {
  override func someMethod() -> String { "SwiftClientSubclass.someMethod()" }
}

print(SwiftClientSubclass().someMethod())
// CHECK: SwiftClientSubclass.someMethod()

