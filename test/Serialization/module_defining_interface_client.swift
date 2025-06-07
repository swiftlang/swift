// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/test-sdk)
// RUN: %empty-directory(%t/test-sdk/usr/lib/swift)
// RUN: %empty-directory(%t/test-sdk/usr/lib/Foo.swiftmodule)
// RUN: %empty-directory(%t/test-sdk/usr/lib/Bar.swiftmodule)
// RUN: %empty-directory(%t/test-sdk/usr/lib/_Foo_Bar.swiftmodule)
// RUN: %empty-directory(%t/test-sdk/usr/lib/Foo.swiftcrossimport)
// RUN: cp -r %platform-module-dir/Swift.swiftmodule %t/test-sdk/usr/lib/swift/Swift.swiftmodule

// RUN: split-file %s %t

// --- Precompile interfaces from the "SDK" into binary modules distributed elsewhere
// RUN: %target-swift-frontend(mock-sdk: -sdk %t/test-sdk) -compile-module-from-interface -module-name Foo %t/test-sdk/usr/lib/Foo.swiftmodule/Foo.swiftinterface -o %t/inputs/Foo.swiftmodule
// RUN: %target-swift-frontend(mock-sdk: -sdk %t/test-sdk) -compile-module-from-interface -module-name Bar %t/test-sdk/usr/lib/Bar.swiftmodule/Bar.swiftinterface -o %t/inputs/Bar.swiftmodule
// RUN: %target-swift-frontend(mock-sdk: -sdk %t/test-sdk) -compile-module-from-interface -module-name _Foo_Bar %t/test-sdk/usr/lib/_Foo_Bar.swiftmodule/_Foo_Bar.swiftinterface -o %t/inputs/_Foo_Bar.swiftmodule

// --- Verify that the client is able to resolve the cross-import overlay from the defining interface SDK location
// RUN: %target-swift-frontend(mock-sdk: -sdk %t/test-sdk) -typecheck %t/Test.swift -enable-cross-import-overlays -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t/inputs -Rmodule-loading &> %t/module_remarks.txt
// RUN: cat %t/module_remarks.txt | %FileCheck %s
// CHECK: remark: loaded module '_Foo_Bar'

//--- test-sdk/usr/lib/Foo.swiftmodule/Foo.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Foo
public func foo()

//--- test-sdk/usr/lib/Bar.swiftmodule/Bar.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Bar
public func bar()

//--- test-sdk/usr/lib/_Foo_Bar.swiftmodule/_Foo_Bar.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name _Foo_Bar
public func foo_bar()

//--- test-sdk/usr/lib/Foo.swiftcrossimport/Bar.swiftoverlay
---
version: 1
modules:
  - name: _Foo_Bar

//--- Test.swift
import Foo
import Bar
public func baz() { foo_bar() }
