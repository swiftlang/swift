// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Module.swiftinterface) %s
// RUN: %target-swift-typecheck-module-from-interface(%t/Module.swiftinterface)
// RUN: %FileCheck %s < %t/Module.swiftinterface

@available(macOS 10.16, iOS 10.16, watchOS 10.16, tvOS 10.16, *)
public func introduced10_16() { }
// CHECK: @available(macOS 11.0, iOS 10.16, watchOS 10.16, tvOS 10.16, *)
// CHECK-NEXT: public func introduced10_16()

@available(OSX 11.0, iOS 11.0, watchOS 11.0, tvOS 11.0, *)
public func introduced11_0() { }
// CHECK-NEXT: @available(macOS 11.0, iOS 11.0, watchOS 11.0, tvOS 11.0, *)
// CHECK-NEXT: public func introduced11_0()

@available(macOS 16.0, iOS 19.0, macCatalyst 19.0, watchOS 12.0, tvOS 19.0, visionOS 3.0, *)
public func introducedInVersionsMappingTo26_0() { }
// CHECK-NEXT: @available(macOS 26.0, iOS 26.0, macCatalyst 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *)
// CHECK-NEXT: public func introducedInVersionsMappingTo26_0()

@available(macOS 17.0, iOS 20.0, macCatalyst 20.0, watchOS 13.0, tvOS 20.0, visionOS 4.0, *)
public func introducedInInvalidVersionsMappingTo27_0() { }
// CHECK-NEXT: @available(macOS 27.0, iOS 27.0, macCatalyst 27.0, watchOS 27.0, tvOS 27.0, visionOS 27.0, *)
// CHECK-NEXT: public func introducedInInvalidVersionsMappingTo27_0()

@available(macOS 19.1.1, iOS 21, macCatalyst 21.0, watchOS 14.5.1, tvOS 21.2.3, visionOS 4.0.1, *)
public func introducedInInvalidVersionsWithVaryingComponents() { }
// CHECK-NEXT: @available(macOS 29.1.1, iOS 28, macCatalyst 28.0, watchOS 28.5.1, tvOS 28.2.3, visionOS 27.0.1, *)
// CHECK-NEXT: public func introducedInInvalidVersionsWithVaryingComponents()

@available(macOS 26.0, iOS 26.0, macCatalyst 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *)
public func introduced26_0() { }
// CHECK-NEXT: @available(macOS 26.0, iOS 26.0, macCatalyst 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *)
// CHECK-NEXT: public func introduced26_0()
