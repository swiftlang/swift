// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library -module-name Test -validate-tbd-against-ir=missing %s -emit-ir -o %t/no-strip.ll
// RUN: %FileCheck %s --input-file %t/no-strip.ll --check-prefixes=CHECK,CHECK-NO-STRIP
// RUN: %FileCheck %s --input-file %t/no-strip.ll --check-prefixes=CHECK-METADATA,CHECK-METADATA-NO-STRIP

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library -module-name Test -validate-tbd-against-ir=missing -unavailable-decl-optimization=complete %s -emit-ir -o %t/strip.ll
// RUN: %FileCheck %s --input-file %t/strip.ll --check-prefixes=CHECK,CHECK-STRIP
// RUN: %FileCheck %s --input-file %t/strip.ll --check-prefixes=CHECK-METADATA,CHECK-METADATA-STRIP

// REQUIRES: objc_interop

import Foundation

public class Class: NSObject {
  // CHECK-NO-STRIP: define {{.*}} @"$s4Test5ClassC3fooyyF"
  // CHECK-STRIP-NOT: define {{.*}} @"$s4Test5ClassC3fooyyF"

  // CHECK-NO-STRIP: define {{.*}} @"$s4Test5ClassC3fooyyFTo"
  // CHECK-STRIP-NOT: define {{.*}} @"$s4Test5ClassC3fooyyFTo"
  @available(*, unavailable)
  @objc public func foo() {}

  @objc public var availableProperty: Int { 0 }

  @available(*, unavailable)
  @objc public var unavailableProperty: Int {
    get { 0 }
    set { }
  }

  @available(*, unavailable)
  @objc public static var unavailableClassProperty: Int { 0 }

  @available(*, unavailable)
  @objc public subscript(unavailableSubscript: Int) -> Int { 0 }

  // CHECK: define {{.*}} @"$s4Test5ClassC29propertyWithUnavailableSetterSivgTo"
  // CHECK-NO-STRIP: define {{.*}} @"$s4Test5ClassC29propertyWithUnavailableSetterSivsTo"
  // CHECK-STRIP-NOT: define {{.*}} @"$s4Test5ClassC29propertyWithUnavailableSetterSivsTo"
  @objc public final var propertyWithUnavailableSetter: Int {
    get { 0 }
    @available(*, unavailable)
    set { }
  }
}

// CHECK-METADATA-NO-STRIP: @_CLASS_PROPERTIES__TtC4Test5Class = internal constant { i32, i32, [1 x { ptr, ptr }] } { i32 {{[0-9]+}}, i32 1, [1 x { ptr, ptr }] [{ ptr, ptr } { ptr @.str.24.unavailableClassProperty, ptr @"{{[^"]*}},N,R" }]
// CHECK-METADATA-STRIP-NOT: @_CLASS_PROPERTIES__TtC4Test5Class

// CHECK-METADATA-NO-STRIP: @_PROPERTIES__TtC4Test5Class = internal constant { i32, i32, [3 x { ptr, ptr }] } { i32 {{[0-9]+}}, i32 3, [3 x { ptr, ptr }] [{ ptr, ptr } { ptr @.str.17.availableProperty, ptr @"{{[^"]*}},N,R" }, { ptr, ptr } { ptr @.str.19.unavailableProperty, ptr @"{{[^"]*}},N" }, { ptr, ptr } { ptr @.str.29.propertyWithUnavailableSetter, ptr @"{{[^"]*}},N" }]
// CHECK-METADATA-STRIP: @_PROPERTIES__TtC4Test5Class = internal constant { i32, i32, [2 x { ptr, ptr }] } { i32 {{[0-9]+}}, i32 2, [2 x { ptr, ptr }] [{ ptr, ptr } { ptr @.str.17.availableProperty, ptr @"{{[^"]*}},N,R" }, { ptr, ptr } { ptr @.str.29.propertyWithUnavailableSetter, ptr @"{{[^"]*}},N,R" }]

// CHECK-METADATA-NO-STRIP: @_PROTOCOL_INSTANCE_METHODS__TtP4Test5Proto_ = weak hidden constant { i32, i32, [7 x { ptr, ptr, ptr }] } { i32 {{[0-9]+}}, i32 7, [7 x { ptr, ptr, ptr }] [{ ptr, ptr, ptr } { ptr @"\01L_selector_data(availableMethod)", {{.*}} }, { ptr, ptr, ptr } { ptr @"\01L_selector_data(unavailableMethod)", {{.*}} }, { ptr, ptr, ptr } { ptr @"\01L_selector_data(availableProperty)", {{.*}} }, { ptr, ptr, ptr } { ptr @"\01L_selector_data(unavailableProperty)", {{.*}} }, { ptr, ptr, ptr } { ptr @"\01L_selector_data(setUnavailableProperty:)", {{.*}} }, { ptr, ptr, ptr } { ptr @"\01L_selector_data(objectAtIndexedSubscript:)", {{.*}} }, { ptr, ptr, ptr } { ptr @"\01L_selector_data(setObject:atIndexedSubscript:)",
// CHECK-METADATA-NO-STRIP: @_PROTOCOL_PROPERTIES__TtP4Test5Proto_ = weak hidden constant { i32, i32, [2 x { ptr, ptr }] } { i32 {{[0-9]+}}, i32 2, [2 x { ptr, ptr }] [{ ptr, ptr } { ptr @.str.17.availableProperty, {{.*}} }, { ptr, ptr } { ptr @.str.19.unavailableProperty,
// CHECK-METADATA-NO-STRIP: @_PROTOCOL_METHOD_TYPES__TtP4Test5Proto_ = weak hidden constant [7 x ptr]

// CHECK-METADATA-STRIP: @_PROTOCOL_INSTANCE_METHODS__TtP4Test5Proto_ = weak hidden constant { i32, i32, [2 x { ptr, ptr, ptr }] } { i32 {{[0-9]+}}, i32 2, [2 x { ptr, ptr, ptr }] [{ ptr, ptr, ptr } { ptr @"\01L_selector_data(availableMethod)", {{.*}} }, { ptr, ptr, ptr } { ptr @"\01L_selector_data(availableProperty)",
// CHECK-METADATA-STRIP: @_PROTOCOL_PROPERTIES__TtP4Test5Proto_ = weak hidden constant { i32, i32, [1 x { ptr, ptr }] } { i32 {{[0-9]+}}, i32 1, [1 x { ptr, ptr }] [{ ptr, ptr } { ptr @.str.17.availableProperty,
// CHECK-METADATA-STRIP: @_PROTOCOL_METHOD_TYPES__TtP4Test5Proto_ = weak hidden constant [2 x ptr] [ptr @{{[^,]+}}, ptr @{{[^,]+}}]
@objc public protocol Proto {
  func availableMethod()

  @available(*, unavailable)
  func unavailableMethod()

  var availableProperty: Int { get }

  @available(*, unavailable)
  var unavailableProperty: Int { get set }

  @available(*, unavailable)
  subscript(unavailableSubscript: Int) -> Int { get set }
}
