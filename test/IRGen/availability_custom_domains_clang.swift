// RUN: %target-swift-emit-irgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-bridging-header %S/Inputs/AvailabilityDomains.h \
// RUN:   -Onone | %FileCheck %s --check-prefixes=CHECK,CHECK-O-NONE

// RUN: %target-swift-emit-irgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -import-bridging-header %S/Inputs/AvailabilityDomains.h \
// RUN:   -O | %FileCheck %s --check-prefixes=CHECK,CHECK-O

// REQUIRES: swift_feature_CustomAvailability

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test24ifAvailableEnabledDomainyyF"()
// CHECK: call void @available_in_enabled_domain()
// CHECK-NOT: call void @unavailable_in_enabled_domain()
public func ifAvailableEnabledDomain() {
  if #available(EnabledDomain) {
    available_in_enabled_domain()
  } else {
    unavailable_in_enabled_domain()
  }
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test25ifAvailableDisabledDomainyyF"()
// CHECK-NOT: call void @available_in_disabled_domain()
// CHECK: call void @unavailable_in_disabled_domain()
public func ifAvailableDisabledDomain() {
  if #available(DisabledDomain) {
    available_in_disabled_domain()
  } else {
    unavailable_in_disabled_domain()
  }
}

// CHECK-LABEL:   define {{.*}}swiftcc void @"$s4Test24ifAvailableDynamicDomainyyF"()
// CHECK:         entry:
// CHECK-O-NONE:    [[QUERY_RESULT:%.*]] = call swiftcc i1 @"$sSC33__swift_DynamicDomain_isAvailableBi1_yF"()
// CHECK-O:         [[PRED_RESULT:%.*]] = {{(tail )?}}call i32 @dynamic_domain_pred()
// CHECK-O:         [[QUERY_RESULT:%.*]] = icmp {{eq|ne}} i32 [[PRED_RESULT]], 0
// CHECK:           br i1 [[QUERY_RESULT]], label %{{.*}}, label %{{.*}}
// CHECK:           call void @available_in_dynamic_domain()
// CHECK:           call void @unavailable_in_dynamic_domain()
public func ifAvailableDynamicDomain() {
  if #available(DynamicDomain) {
    available_in_dynamic_domain()
  } else {
    unavailable_in_dynamic_domain()
  }
}

// CHECK-O-NONE-LABEL: define {{.*}}swiftcc i1 @"$sSC33__swift_DynamicDomain_isAvailableBi1_yF"()
// CHECK-O-NONE:       entry:
// CHECK-O-NONE:         [[CALL:%.*]] = call i32 @dynamic_domain_pred()
// CHECK-O-NONE:         [[TOBOOL:%.*]] = icmp ne i32 [[CALL]], 0
// CHECK-O-NONE:         ret i1 [[TOBOOL]]
