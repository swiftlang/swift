// RUN: %empty-directory(%t)

// Ensure the attribute is printed in swiftinterface files
// RUN: %target-swift-frontend-typecheck -emit-module-interface-path %t/Foo.swiftinterface %s -module-name Foo
// RUN: %FileCheck %s < %t/Foo.swiftinterface

// Ensure the attribute is in .swiftmodule files
// RUN: %target-swift-ide-test -print-module -module-to-print Foo -I %t -source-filename %s > %t/printed-module.txt
// RUN: %FileCheck %s < %t/printed-module.txt

// CHECK: @_originallyDefinedIn(module: "another", OSX 13.13)
@_originallyDefinedIn(module: "another", OSX 13.13)
public protocol SimpleProto { }

// CHECK: @_originallyDefinedIn(module: "original", tvOS 1.0)
// CHECK: @_originallyDefinedIn(module: "another_original", OSX 2.0)
// CHECK: @_originallyDefinedIn(module: "another_original", iOS 3.0)
// CHECK: @_originallyDefinedIn(module: "another_original", watchOS 4.0)
@_originallyDefinedIn(module: "original", tvOS 1.0)
@_originallyDefinedIn(module: "another_original", OSX 2.0, iOS 3.0, watchOS 4.0)
public struct SimpleStruct {}
