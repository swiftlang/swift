// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/RepeatedAvailability.swiftinterface) %s \
// RUN:   -define-availability 'MyLib_v1_0:iOS 13.0, macOS 10.15, tvOS 13.0, watchOS 6.0'
// RUN: %target-swift-typecheck-module-from-interface(%t/RepeatedAvailability.swiftinterface)
// RUN: %FileCheck %s < %t/RepeatedAvailability.swiftinterface

// A library's availability macro can overlap with an explicit platform
// requirement, regardless of which attribute appears first.
@available(macOS 15.0, *)
@available(MyLib_v1_0, *)
public func api() {}
// CHECK: @available(macOS 15.0, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
// CHECK-NEXT: public func api()

@available(MyLib_v1_0, *)
@available(macOS 15.0, *)
public func apiWithMacroFirst() {}
// CHECK-NEXT: @available(iOS 13.0, macOS 15.0, tvOS 13.0, watchOS 6.0, *)
// CHECK-NEXT: public func apiWithMacroFirst()

// Repeated platforms must appear only once in the combined short-form
// attribute, keeping the strongest introduction requirement in either order.
@available(macOS 12.0, *)
@available(macOS 13.0, *)
public func increasingIntroductions() {}
// CHECK-NEXT: @available(macOS 13.0, *)
// CHECK-NEXT: public func increasingIntroductions()

@available(macOS 13.0, *)
@available(macOS 12.0, *)
public func decreasingIntroductions() {}
// CHECK-NEXT: @available(macOS 13.0, *)
// CHECK-NEXT: public func decreasingIntroductions()

@available(macOS 13.0, *)
@available(macOS 13.0, *)
public func identicalIntroductions() {}
// CHECK-NEXT: @available(macOS 13.0, *)
// CHECK-NEXT: public func identicalIntroductions()

// Long-form attributes containing only an introduction version also coalesce.
@available(macOS, introduced: 12.0)
@available(macOS, introduced: 13.0)
public func longFormIntroductions() {}
// CHECK-NEXT: @available(macOS 13.0, *)
// CHECK-NEXT: public func longFormIntroductions()

@available(macOS, introduced: 13.0)
@available(macOS 12.0, *)
public func mixedFormIntroductions() {}
// CHECK-NEXT: @available(macOS 13.0, *)
// CHECK-NEXT: public func mixedFormIntroductions()

// Merging overlapping groups preserves the order of the first occurrence of
// each platform, including when a later attribute raises its version.
@available(macOS 12.0, iOS 13.0, tvOS 14.0, *)
@available(iOS 15.0, watchOS 8.0, macOS 13.0, *)
public func overlappingPlatforms() {}
// CHECK-NEXT: @available(macOS 13.0, iOS 15.0, tvOS 14.0, watchOS 8.0, *)
// CHECK-NEXT: public func overlappingPlatforms()

// Platform aliases identify the same availability domain.
@available(OSX 10.15, *)
@available(macOS 12.0, *)
public func platformAliases() {}
// CHECK-NEXT: @available(macOS 12.0, *)
// CHECK-NEXT: public func platformAliases()

// Attributes carrying additional information must remain separate.
@available(macOS 12.0, *)
@available(macOS, introduced: 11.0, deprecated: 14.0, message: "Use replacement")
@available(macOS 13.0, *)
public func preservedDeprecation() {}
// CHECK-NEXT: @available(macOS 13.0, *)
// CHECK-NEXT: @available(macOS, introduced: 11.0, deprecated: 14.0, message: "Use replacement")
// CHECK-NEXT: public func preservedDeprecation()
