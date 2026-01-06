// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/test-sdk)
// RUN: %empty-directory(%t/test-sdk/usr/lib/swift)
// RUN: cp -r %platform-module-dir/Swift.swiftmodule %t/test-sdk/usr/lib/swift/Swift.swiftmodule

// RUN: %empty-directory(%t/test-sdk/usr/lib/Foo.swiftmodule)
// RUN: split-file %s %t

// --- Verify that a non-SDK module gets its absolute path serialized into the binary '.swiftmodule'
// RUN: %target-swift-frontend -compile-module-from-interface -module-name Foo %t/modules/Foo.swiftinterface -o %t/inputs/FreestandingFoo.swiftmodule
// RUN: llvm-bcanalyzer -dump %t/inputs/FreestandingFoo.swiftmodule > %t/Foo.freestanding.moduledump.txt
// RUN: cat %t/Foo.freestanding.moduledump.txt | %FileCheck %s -check-prefix CHECK-FREESTANDING-FOO

// --- Verify that an SDK module gets its SDK-relative path serialized into the binary '.swiftmodule'
// RUN: cp %t/modules/Foo.swiftinterface %t/test-sdk/usr/lib/Foo.swiftmodule
// RUN: %target-swift-frontend(mock-sdk: -sdk %t/test-sdk) -compile-module-from-interface -module-name Foo %t/test-sdk/usr/lib/Foo.swiftmodule/Foo.swiftinterface -o %t/inputs/Foo.swiftmodule
// RUN: llvm-bcanalyzer -dump %t/inputs/Foo.swiftmodule > %t/Foo.sdk.moduledump.txt
// RUN: cat %t/Foo.sdk.moduledump.txt | %FileCheck %s -check-prefix CHECK-SDK-FOO

// CHECK-FREESTANDING-FOO: <MODULE_INTERFACE_PATH abbrevid={{[0-9]+}} op0=0/> blob data = '{{.*}}{{/|\\}}modules{{/|\\}}Foo.swiftinterface'
// CHECK-SDK-FOO: <MODULE_INTERFACE_PATH abbrevid={{[0-9]+}} op0=1/> blob data = 'usr/lib/Foo.swiftmodule/Foo.swiftinterface'

//--- modules/Foo.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Foo
public func foo()
