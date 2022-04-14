// RUN: %empty-directory(%t)

// Ensure the attribute is printed in swiftinterface files
// RUN: %target-swift-emit-module-interface(%t/Foo.swiftinterface) %s -module-name Foo \
// RUN:   -define-availability "_iOS8Aligned:macOS 10.10, iOS 8.0" \
// RUN:   -define-availability "_iOS9:iOS 9.0" \
// RUN:   -define-availability "_macOS10_11:macOS 10.11" \
// RUN:   -define-availability "_myProject 1.0:macOS 10.11"
// RUN: %target-swift-typecheck-module-from-interface(%t/Foo.swiftinterface) -module-name Foo
// RUN: %FileCheck %s < %t/Foo.swiftinterface

// Ensure the attribute is in .swiftmodule files
// RUN: %target-swift-ide-test -print-module -module-to-print Foo -I %t -source-filename %s > %t/printed-module.txt
// RUN: %FileCheck %s < %t/printed-module.txt

// CHECK: @_originallyDefinedIn(module: "another", macOS 13.13)
@available(OSX 10.8, *)
@_originallyDefinedIn(module: "another", OSX 13.13)
public protocol SimpleProto { }

// CHECK: @_originallyDefinedIn(module: "original", tvOS 1.0)
// CHECK: @_originallyDefinedIn(module: "another_original", macOS 2.0)
// CHECK: @_originallyDefinedIn(module: "another_original", iOS 3.0)
// CHECK: @_originallyDefinedIn(module: "another_original", watchOS 4.0)
@available(tvOS 0.7, OSX 1.1, iOS 2.1, watchOS 3.2, *)
@_originallyDefinedIn(module: "original", tvOS 1.0)
@_originallyDefinedIn(module: "another_original", OSX 2.0, iOS 3.0, watchOS 4.0)
public struct SimpleStruct {}

// CHECK: @_originallyDefinedIn(module: "other0", macOS 10.10)
// CHECK: @_originallyDefinedIn(module: "other0", iOS 8.0)
@available(tvOS 0.7, OSX 1.1, iOS 2.1, watchOS 3.2, *)
@_originallyDefinedIn(module: "other0", _iOS8Aligned)
public struct SimpleThingInAlphabeticalOrderForMacros0 {}

// CHECK: @_originallyDefinedIn(module: "other1", iOS 9.0)
// CHECK: @_originallyDefinedIn(module: "other1", macOS 10.11)
@available(tvOS 0.7, OSX 1.1, iOS 2.1, watchOS 3.2, *)
@_originallyDefinedIn(module: "other1", _iOS9, _macOS10_11)
public struct SimpleThingInAlphabeticalOrderForMacros1 {}

// CHECK: @_originallyDefinedIn(module: "other2", macOS 10.11)
@available(tvOS 0.7, OSX 1.1, iOS 2.1, watchOS 3.2, *)
@_originallyDefinedIn(module: "other2", _myProject 1.0)
public struct SimpleThingInAlphabeticalOrderForMacros2 {}

// CHECK: @_originallyDefinedIn(module: "another", macOS 13.13)
@available(OSX 10.8, *)
@_originallyDefinedIn(module: "another", OSX 13.13)
@usableFromInline
internal struct SimpleThingInAlphabeticalOrderForMacros3_UsableFromInline {}
