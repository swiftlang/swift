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
// CHECK-LABEL: protocol SimpleProto
@available(OSX 10.8, *)
@_originallyDefinedIn(module: "another", OSX 13.13)
public protocol SimpleProto { }

// CHECK: @_originallyDefinedIn(module: "original", tvOS 1.0)
// CHECK: @_originallyDefinedIn(module: "another_original", macOS 2.0)
// CHECK: @_originallyDefinedIn(module: "another_original", iOS 3.0)
// CHECK: @_originallyDefinedIn(module: "another_original", watchOS 4.0)
// CHECK-LABEL: struct SimpleStruct
@available(tvOS 0.7, OSX 1.1, iOS 2.1, watchOS 3.2, *)
@_originallyDefinedIn(module: "original", tvOS 1.0)
@_originallyDefinedIn(module: "another_original", OSX 2.0, iOS 3.0, watchOS 4.0)
public struct SimpleStruct {}

// CHECK: @_originallyDefinedIn(module: "other0", macOS 10.10)
// CHECK: @_originallyDefinedIn(module: "other0", iOS 8.0)
// CHECK-LABEL: struct SimpleThingInAlphabeticalOrderForMacros0
@available(tvOS 0.7, OSX 1.1, iOS 2.1, watchOS 3.2, *)
@_originallyDefinedIn(module: "other0", _iOS8Aligned)
public struct SimpleThingInAlphabeticalOrderForMacros0 {}

// CHECK: @_originallyDefinedIn(module: "other1", iOS 9.0)
// CHECK: @_originallyDefinedIn(module: "other1", macOS 10.11)
// CHECK-LABEL: struct SimpleThingInAlphabeticalOrderForMacros1
@available(tvOS 0.7, OSX 1.1, iOS 2.1, watchOS 3.2, *)
@_originallyDefinedIn(module: "other1", _iOS9, _macOS10_11)
public struct SimpleThingInAlphabeticalOrderForMacros1 {}

// CHECK: @_originallyDefinedIn(module: "other2", macOS 10.11)
// CHECK-LABEL: struct SimpleThingInAlphabeticalOrderForMacros2
@available(tvOS 0.7, OSX 1.1, iOS 2.1, watchOS 3.2, *)
@_originallyDefinedIn(module: "other2", _myProject 1.0)
public struct SimpleThingInAlphabeticalOrderForMacros2 {}

// CHECK: @_originallyDefinedIn(module: "another", macOS 13.13)
// CHECK-LABEL: struct SimpleThingInAlphabeticalOrderForMacros3_UsableFromInline
@available(OSX 10.8, *)
@_originallyDefinedIn(module: "another", OSX 13.13)
@usableFromInline
internal struct SimpleThingInAlphabeticalOrderForMacros3_UsableFromInline {}

// CHECK: @_originallyDefinedIn(module: "pre26", macOS 26.0)
// CHECK: @_originallyDefinedIn(module: "pre26", iOS 26.0)
// CHECK: @_originallyDefinedIn(module: "pre26", watchOS 26.0)
// CHECK: @_originallyDefinedIn(module: "pre26", tvOS 26.0)
// CHECK: @_originallyDefinedIn(module: "pre26", visionOS 26.0)
// CHECK-LABEL: struct SimpleThingInAlphabeticalOrderForMacros4_VersionsMappingTo26
@available(macOS 15, iOS 18, watchOS 11, tvOS 18, visionOS 2, *)
@_originallyDefinedIn(module: "pre26", macOS 16, iOS 19, watchOS 12, tvOS 19, visionOS 3)
public struct SimpleThingInAlphabeticalOrderForMacros4_VersionsMappingTo26 {}

// CHECK: @_originallyDefinedIn(module: "pre27", macOS 27)
// CHECK: @_originallyDefinedIn(module: "pre27", iOS 27)
// CHECK: @_originallyDefinedIn(module: "pre27", watchOS 27)
// CHECK: @_originallyDefinedIn(module: "pre27", tvOS 27)
// CHECK: @_originallyDefinedIn(module: "pre27", visionOS 27)
// CHECK-LABEL: struct SimpleThingInAlphabeticalOrderForMacros5_VersionsMappingTo27
@available(macOS 15, iOS 18, watchOS 11, tvOS 18, visionOS 2, *)
@_originallyDefinedIn(module: "pre27", macOS 17, iOS 20, watchOS 13, tvOS 20, visionOS 4)
public struct SimpleThingInAlphabeticalOrderForMacros5_VersionsMappingTo27 {}

// CHECK: @_originallyDefinedIn(module: "pre26", macOS 26)
// CHECK: @_originallyDefinedIn(module: "pre26", iOS 26)
// CHECK: @_originallyDefinedIn(module: "pre26", watchOS 26)
// CHECK: @_originallyDefinedIn(module: "pre26", tvOS 26)
// CHECK: @_originallyDefinedIn(module: "pre26", visionOS 26)
// CHECK-LABEL: struct SimpleThingInAlphabeticalOrderForMacros6_Version26
@available(macOS 15, iOS 18, watchOS 11, tvOS 18, visionOS 2, *)
@_originallyDefinedIn(module: "pre26", macOS 26, iOS 26, watchOS 26, tvOS 26, visionOS 26)
public struct SimpleThingInAlphabeticalOrderForMacros6_Version26 {}
