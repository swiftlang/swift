// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t  %S/Inputs/ne_types.swift \
// RUN: -enable-experimental-feature LifetimeDependence \
// RUN: -enable-experimental-feature AddressableTypes \
// RUN: -enable-library-evolution \
// RUN: -emit-module-path %t/ne_types.swiftmodule 

// RUN: %target-swift-frontend -emit-silgen -I %t %s \
// RUN: -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_AddressableTypes

import ne_types

// Ensure no memory crashes during SILGen

struct NonEscapableFrameworkThingTests {
  func example() async throws {
    let ptr = UnsafeMutableRawBufferPointer.allocate(byteCount: 40, alignment: 1)
    defer { ptr.deallocate() }
    var something = Something(ptr: ptr)
    var mutableView = something.mutableView(of: Float32.self)
    var mutableSpan = mutableView.mutableSpan
  }
}
