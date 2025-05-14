// RUN: %target-swift-frontend -module-name A -emit-ir -primary-file %s \
// RUN: -enable-experimental-feature LifetimeDependence \
// RUN: -enable-experimental-feature AddressableTypes \
// RUN: | %FileCheck %s

// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_LifetimeDependence

public struct NE: ~Escapable {}

@_addressableForDependencies
public struct Holder {}

@_silgen_name("holder_NE")
@lifetime(borrow holder)
func getNE(_ holder: Holder) -> NE

@_silgen_name("holder_mut_NE")
@lifetime(&holder)
func getMutNE(_ holder: inout Holder) -> NE

// The parameter cannot be 'nocapture'.
//
// CHECK-LABEL: define{{.*}} swiftcc void @"$s1A17testAddressableInyAA2NEVAA6HolderVF"(ptr noalias %0)
public func testAddressableIn(_ holder: Holder) -> NE {
  getNE(holder)
}

// The parameter cannot be 'nocapture'.
//
// CHECK-LABEL: define{{.*}} swiftcc void @"$s1A20testAddressableInoutyAA2NEVAA6HolderVzF"(ptr %0)
public func testAddressableInout(_ holder: inout Holder) -> NE {
  getMutNE(&holder)
}
