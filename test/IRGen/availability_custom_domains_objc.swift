// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir %s \
// RUN:   -module-name Test -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-bridging-header %S/Inputs/AvailabilityDomainsObjC.h \
// RUN:   -disable-objc-attr-requires-foundation-module \
// RUN:   -Onone > %t.ir
// RUN: %FileCheck %s --input-file %t.ir \
// RUN:   --implicit-check-not=DisabledDomain \
// RUN:   --implicit-check-not=disabledDomain \
// RUN:   --implicit-check-not=setPropertyWithDisabledSetter \
// RUN:   --implicit-check-not=propertyWithDisabledSetterSivs \
// RUN:   --implicit-check-not=propertyWithDisabledSetterSivM
// RUN: %FileCheck %s --input-file %t.ir --check-prefix=CHECK-METADATA

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

  // CHECK: define internal {{.*}} @"$s4Test20AlwaysAvailableClassC21enabledDomainPropertySivgTo"
  @available(EnabledDomain)
  @objc public var enabledDomainProperty: Int { 0 }

  @available(DisabledDomain)
  @objc public var disabledDomainProperty: Int {
    get { 0 }
    set { }
  }

  @available(DisabledDomain)
  @objc public static var disabledDomainClassProperty: Int { 0 }

  @available(DisabledDomain)
  @objc public subscript(disabledDomainSubscript: Int) -> Int { 0 }

  @objc public final var propertyWithDisabledSetter: Int {
    get { 0 }
    @available(DisabledDomain)
    set { }
  }
}

// CHECK-METADATA: @_PROPERTIES__TtC4Test20AlwaysAvailableClass = internal constant { i32, i32, [2 x { ptr, ptr }] } { i32 {{[0-9]+}}, i32 2, [2 x { ptr, ptr }] [{ ptr, ptr } { ptr @.str.21.enabledDomainProperty, ptr @"{{.*}},R" }, { ptr, ptr } { ptr @.str.26.propertyWithDisabledSetter, ptr @"{{.*}},R" }]
// CHECK-METADATA: @_PROTOCOL_INSTANCE_METHODS__TtP4Test20AlwaysAvailableProto_ = weak hidden constant { i32, i32, [2 x { ptr, ptr, ptr }] } { i32 {{[0-9]+}}, i32 2, [2 x { ptr, ptr, ptr }] [{ ptr, ptr, ptr } { ptr @"\01L_selector_data(enabledDomainMethod)", {{.*}} }, { ptr, ptr, ptr } { ptr @"\01L_selector_data(enabledDomainProperty)",
// CHECK-METADATA: @_PROTOCOL_PROPERTIES__TtP4Test20AlwaysAvailableProto_ = weak hidden constant { i32, i32, [1 x { ptr, ptr }] } { i32 {{[0-9]+}}, i32 1, [1 x { ptr, ptr }] [{ ptr, ptr } { ptr @.str.21.enabledDomainProperty,
// CHECK-METADATA: @_PROTOCOL_METHOD_TYPES__TtP4Test20AlwaysAvailableProto_ = weak hidden constant [2 x ptr] [ptr @{{[^,]+}}, ptr @{{[^,]+}}]
@objc public protocol AlwaysAvailableProto {
  @available(EnabledDomain)
  func enabledDomainMethod()

  @available(DisabledDomain)
  func disabledDomainMethod()

  @available(EnabledDomain)
  var enabledDomainProperty: Int { get }

  @available(DisabledDomain)
  var disabledDomainProperty: Int { get set }

  @available(DisabledDomain)
  subscript(disabledDomainSubscript: Int) -> Int { get set }
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
