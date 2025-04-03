// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -module-name unsafe -emit-module -o %t/unsafe.swiftmodule -emit-module-interface-path - %s | %FileCheck %s

// CHECK: #if compiler(>=5.3) && $MemorySafetyAttributes
// CHECK: @unsafe public func testFunction()
// CHECK: #else
// CHECK: public func testFunction()
// CHECK: #endif
@unsafe public func testFunction() { }
