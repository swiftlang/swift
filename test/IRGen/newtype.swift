// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t -I %S/../IDE/Inputs/custom-modules) %s -emit-ir -enable-swift-newtype | FileCheck %s
import CoreFoundation
import Foundation
import Newtype

// REQUIRES: objc_interop

// Witness table for synthesized ClosedEnums : _ObjectiveCBridgeable.
// CHECK: @_TWPVSC10ClosedEnums21_ObjectiveCBridgeable7Newtype = linkonce_odr

// CHECK-LABEL: define %CSo8NSString* @_TF7newtype14getErrorDomainFT_VSC11ErrorDomain()
public func getErrorDomain() -> ErrorDomain {
  // CHECK: load %CSo8NSString*, %CSo8NSString** getelementptr inbounds (%VSC11ErrorDomain, %VSC11ErrorDomain* @SNTErrOne
  return .one
}

// CHECK-LABEL: _TF7newtype6getFooFT_VSC18NSNotificationName
public func getFoo() -> NSNotificationName {
  return NSNotificationName.Foo
  // CHECK: load {{.*}} @FooNotification
  // CHECK: ret
}

// CHECK-LABEL: _TF7newtype21getGlobalNotificationFSiSS
public func getGlobalNotification(_ x: Int) -> String {
  switch x {
    case 1: return kNotification
    // CHECK: load {{.*}} @kNotification
    case 2: return Notification
    // CHECK: load {{.*}} @Notification
    case 3: return swiftNamedNotification
    // CHECK: load {{.*}} @kSNNotification
    default: return NSNotificationName.bar.rawValue
    // CHECK: load {{.*}} @kBarNotification
  }
// CHECK: ret
}

// CHECK-LABEL: _TF7newtype17getCFNewTypeValueFT6useVarSb_VSC9CFNewType
public func getCFNewTypeValue(useVar: Bool) -> CFNewType {
  if (useVar) {
    return CFNewType.MyCFNewTypeValue
    // CHECK: load {{.*}} @MyCFNewTypeValue
  } else {
    return FooAudited()
    // CHECK: call {{.*}} @FooAudited()
  }
  // CHECK: ret
}

// CHECK-LABEL: _TF7newtype21getUnmanagedCFNewTypeFT6useVarSb_GVs9UnmanagedCSo8CFString_
public func getUnmanagedCFNewType(useVar: Bool) -> Unmanaged<CFString> {
  if (useVar) {
    return CFNewType.MyCFNewTypeValueUnaudited
    // CHECK: load {{.*}} @MyCFNewTypeValueUnaudited
  } else {
    return FooUnaudited()
    // CHECK: call {{.*}} @FooUnaudited()
  }
  // CHECK: ret
}

// Triggers instantiation of ClosedEnum : _ObjectiveCBridgeable
// witness table.
public func hasArrayOfClosedEnums(closed: [ClosedEnum]) {
}

// CHECK-LABEL: _TF7newtype11compareABIsFT_T_
public func compareABIs() {
  let new = getMyABINewType()
  let old = getMyABIOldType()
  takeMyABINewType(new)
  takeMyABIOldType(old)

  takeMyABINewTypeNonNull(new!)
  takeMyABIOldTypeNonNull(old!)

  let newNS = getMyABINewTypeNS()
  let oldNS = getMyABIOldTypeNS()
  takeMyABINewTypeNonNullNS(newNS!)
  takeMyABIOldTypeNonNullNS(oldNS!)

  // Make sure that the calling conventions align correctly, that is we don't
  // have double-indirection or anything else like that
  // CHECK: declare %struct.__CFString* @getMyABINewType() #0
  // CHECK: declare %struct.__CFString* @getMyABIOldType() #0
  //
  // CHECK: declare void @takeMyABINewType(%struct.__CFString*) #0
  // CHECK: declare void @takeMyABIOldType(%struct.__CFString*) #0
  //
  // CHECK: declare void @takeMyABINewTypeNonNull(%struct.__CFString*) #0
  // CHECK: declare void @takeMyABIOldTypeNonNull(%struct.__CFString*) #0
  //
  // CHECK: declare %0* @getMyABINewTypeNS() #0
  // CHECK: declare %0* @getMyABIOldTypeNS() #0
  //
  // CHECK: declare void @takeMyABINewTypeNonNullNS(%0*) #0
  // CHECK: declare void @takeMyABIOldTypeNonNullNS(%0*) #0
}
