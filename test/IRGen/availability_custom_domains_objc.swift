// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir %s \
// RUN:   -module-name Test -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-bridging-header %S/Inputs/AvailabilityDomainsObjC.h \
// RUN:   -disable-objc-attr-requires-foundation-module \
// RUN:   -Onone > %t.ir
// RUN: %FileCheck %s --input-file %t.ir \
// RUN:   --implicit-check-not=DisabledDomain \
// RUN:   --implicit-check-not=disabledDomain

// REQUIRES: objc_interop
// REQUIRES: swift_feature_CustomAvailability

// The absence of symbols protected by @available(DisabledDomain) is verified
// using '--implicit-check-not'.

// CHECK: @"OBJC_METACLASS_$__TtC4Test22EnabledDomainObjCClass"
// CHECK: @"OBJC_METACLASS_$__TtC4Test22DynamicDomainObjCClass"
// CHECK: @"OBJC_METACLASS_$_EnabledDomainObjCImplementationClass"
// CHECK: @"OBJC_METACLASS_$_DynamicDomainObjCImplementationClass"

@objc public class AlwaysAvailableClass {
  // CHECK: define internal void @"$s4Test20AlwaysAvailableClassC19enabledDomainMethodyyFTo"
  @available(EnabledDomain)
  @objc public func enabledDomainMethod() { }

  // CHECK: define internal void @"$s4Test20AlwaysAvailableClassC19dynamicDomainMethodyyFTo"
  @available(DynamicDomain)
  @objc public func dynamicDomainMethod() { }

  @available(DisabledDomain)
  @objc func disabledDomainMethod() { }
}

@available(EnabledDomain)
@objc public class EnabledDomainObjCClass { }

@available(DynamicDomain)
@objc public class DynamicDomainObjCClass { }

@available(DisabledDomain)
@objc public class DisabledDomainObjCClass { }

@objc @implementation
extension AlwaysAvailableObjCImplementationClass {
  // CHECK: define internal void @"$sSo38AlwaysAvailableObjCImplementationClassC4TestE19enabledDomainMethodyyFTo"
  @available(EnabledDomain)
  func enabledDomainMethod() { }

  // CHECK: define internal void @"$sSo38AlwaysAvailableObjCImplementationClassC4TestE19dynamicDomainMethodyyFTo"
  @available(DynamicDomain)
  func dynamicDomainMethod() { }

  @available(DisabledDomain)
  func disabledDomainMethod() { }
}

@available(EnabledDomain)
@objc @implementation
extension EnabledDomainObjCImplementationClass { }

@available(DynamicDomain)
@objc @implementation
extension DynamicDomainObjCImplementationClass { }

@available(DisabledDomain)
@objc @implementation
extension DisabledDomainObjCImplementationClass { }

// CHECK: define swiftcc %swift.metadata_response @"$s4Test22EnabledDomainObjCClassCMa"
// CHECK: define swiftcc %swift.metadata_response @"$s4Test22DynamicDomainObjCClassCMa"
// CHECK: define swiftcc %swift.metadata_response @"$sSo36EnabledDomainObjCImplementationClassCMa"
// CHECK: define swiftcc %swift.metadata_response @"$sSo36DynamicDomainObjCImplementationClassCMa"
