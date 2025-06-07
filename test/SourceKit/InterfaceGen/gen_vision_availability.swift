// REQUIRES: concurrency

// RUN: %empty-directory(%t)

// RUN: %sourcekitd-test -req=interface-gen %s -- %s -target %target-triple -module-name MyModule | %FileCheck %s --check-prefix=SWIFTSOURCE

// RUN: %target-swift-frontend -emit-module -module-name MyModule -o %t/MyModule.swiftmodule %s
// RUN: %sourcekitd-test -req=interface-gen -module MyModule -- -target %target-triple -I %t | %FileCheck %s --check-prefix=SWIFTMODULE
// RUN: %sourcekitd-test -req=doc-info -module MyModule -- -target %target-triple -I %t | %FileCheck %s --check-prefix=DOCINFO

@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, visionOS 1.0, *)
public struct SomeStruct {}

public struct TestStruct {
  @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, visionOS 1.0, *)
  public func availableMethod() {}
}

// SWIFTSOURCE: @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, visionOS 1.0, *)
// SWIFTSOURCE: public struct SomeStruct {
// SWIFTSOURCE: }
// SWIFTSOURCE: public struct TestStruct {
// SWIFTSOURCE:     @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, visionOS 1.0, *)
// SWIFTSOURCE:     public func availableMethod()
// SWIFTSOURCE: }

// SWIFTMODULE: @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, visionOS 1.0, *)
// SWIFTMODULE: public struct SomeStruct {
// SWIFTMODULE: }
// SWIFTMODULE: public struct TestStruct {
// SWIFTMODULE:         @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, visionOS 1.0, *)
// SWIFTMODULE:         public func availableMethod()
// SWIFTMODULE: }

// DOCINFO: struct SomeStruct {
// DOCINFO: }
// DOCINFO: struct TestStruct {
// DOCINFO:      func availableMethod()
// DOCINFO: }
