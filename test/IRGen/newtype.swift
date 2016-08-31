// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t -I %S/../IDE/Inputs/custom-modules) %s -emit-ir -enable-swift-newtype | %FileCheck %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t -I %S/../IDE/Inputs/custom-modules) %s -emit-ir -enable-swift-newtype -O | %FileCheck %s -check-prefix=OPT
import CoreFoundation
import Foundation
import Newtype

// REQUIRES: objc_interop

// Witness table for synthesized ClosedEnums : _ObjectiveCBridgeable.
// CHECK: @_TWPVSC10ClosedEnums21_ObjectiveCBridgeable7Newtype = linkonce_odr

// CHECK-LABEL: define %CSo8NSString* @_TF7newtype14getErrorDomainFT_VSC11ErrorDomain()
public func getErrorDomain() -> ErrorDomain {
  // CHECK: load %CSo8NSString*, %CSo8NSString** getelementptr inbounds (%VSC11ErrorDomain, %VSC11ErrorDomain* {{.*}}@SNTErrOne
  return .one
}

// CHECK-LABEL: _TF7newtype6getFooFT_VCSo14NSNotification4Name
public func getFoo() -> NSNotification.Name {
  return NSNotification.Name.Foo
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
    default: return NSNotification.Name.bar.rawValue
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
  // CHECK: declare %struct.__CFString* @getMyABINewType()
  // CHECK: declare %struct.__CFString* @getMyABIOldType()
  //
  // CHECK: declare void @takeMyABINewType(%struct.__CFString*)
  // CHECK: declare void @takeMyABIOldType(%struct.__CFString*)
  //
  // CHECK: declare void @takeMyABINewTypeNonNull(%struct.__CFString*)
  // CHECK: declare void @takeMyABIOldTypeNonNull(%struct.__CFString*)
  //
  // CHECK: declare %0* @getMyABINewTypeNS()
  // CHECK: declare %0* @getMyABIOldTypeNS()
  //
  // CHECK: declare void @takeMyABINewTypeNonNullNS(%0*)
  // CHECK: declare void @takeMyABIOldTypeNonNullNS(%0*)
}

// OPT-LABEL: define i1 @_TF7newtype12compareInitsFT_Sb
public func compareInits() -> Bool {
  let mf = MyInt(rawValue: 1)
  let mfNoLabel = MyInt(1)
  let res = mf.rawValue == MyInt.one.rawValue 
        && mfNoLabel.rawValue == MyInt.one.rawValue
  // OPT:  [[ONE:%.*]] = load i32, i32*{{.*}}@kMyIntOne{{.*}}, align 4
  // OPT-NEXT: [[COMP:%.*]] = icmp eq i32 [[ONE]], 1

  takesMyInt(mf)
  takesMyInt(mfNoLabel)
  takesMyInt(MyInt(rawValue: kRawInt))
  takesMyInt(MyInt(kRawInt))
  // OPT: tail call void @takesMyInt(i32 1)
  // OPT-NEXT: tail call void @takesMyInt(i32 1)
  // OPT-NEXT: [[RAWINT:%.*]] = load i32, i32*{{.*}} @kRawInt{{.*}}, align 4
  // OPT-NEXT: tail call void @takesMyInt(i32 [[RAWINT]])
  // OPT-NEXT: tail call void @takesMyInt(i32 [[RAWINT]])

  return res
  // OPT-NEXT: ret i1 [[COMP]]
}

// CHECK-LABEL: anchor
// OPT-LABEL: anchor
public func anchor() -> Bool {
  return false
}

class ObjCTest {
  // CHECK-LABEL: define hidden %0* @_TToFC7newtype8ObjCTest19optionalPassThroughfGSqVSC11ErrorDomain_GSqS1__
  // CHECK: [[CASTED:%.+]] = ptrtoint %0* %2 to i{{32|64}}
  // CHECK: [[RESULT:%.+]] = call i{{32|64}} @_TFC7newtype8ObjCTest19optionalPassThroughfGSqVSC11ErrorDomain_GSqS1__(i{{32|64}} [[CASTED]], %C7newtype8ObjCTest* {{%.+}})
  // CHECK: [[OPAQUE_RESULT:%.+]] = inttoptr i{{32|64}} [[RESULT]] to %0*
  // CHECK: ret %0* [[OPAQUE_RESULT]]
  // CHECK: {{^}$}}

  // OPT-LABEL: define hidden %0* @_TToFC7newtype8ObjCTest19optionalPassThroughfGSqVSC11ErrorDomain_GSqS1__
  // OPT: ret %0* %2
  // OPT: {{^}$}}
  @objc func optionalPassThrough(_ ed: ErrorDomain?) -> ErrorDomain? {
    return ed
  }

  // CHECK-LABEL: define hidden i32 @_TToFC7newtype8ObjCTest18integerPassThroughfVSC5MyIntS1_
  // CHECK: [[RESULT:%.+]] = call i32 @_TFC7newtype8ObjCTest18integerPassThroughfVSC5MyIntS1_(i32 %2, %C7newtype8ObjCTest* {{%.+}})
  // CHECK: ret i32 [[RESULT]]
  // CHECK: {{^}$}}

  // OPT-LABEL: define hidden i32 @_TToFC7newtype8ObjCTest18integerPassThroughfVSC5MyIntS1_
  // OPT: ret i32 %2
  // OPT: {{^}$}}
  @objc func integerPassThrough(_ num: MyInt) -> MyInt {
    return num
  }
}