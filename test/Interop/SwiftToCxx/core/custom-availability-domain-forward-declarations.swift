// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Availability -typecheck -verify -enable-experimental-feature CustomAvailability -clang-header-expose-decls=all-public -emit-clang-header-nonmodular-includes -I %S/../../../Inputs/custom-modules/availability-domains -emit-clang-header-path %t/availability.h
// RUN: %FileCheck %s < %t/availability.h
// RUN: %check-interop-cxx-header-in-clang(%t/availability.h -I %S/../../../Inputs/custom-modules/availability-domains -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: swift_feature_CustomAvailability

import Seas

// Custom availability attributes need their imported domain definitions even
// on forward declarations. Keep these declarations after the domain's header.
// CHECK-NOT: class SWIFT_AVAILABILITY_DOMAIN
// CHECK: #include <Seas.h>
// CHECK: class SWIFT_AVAILABILITY_DOMAIN(Bering,0) SWIFT_SYMBOL("{{.*}}") DomainClass;
// CHECK: class SWIFT_AVAILABILITY_DOMAIN(Bering,0) SWIFT_SYMBOL("{{.*}}") DomainValue;

@available(Bering)
public class DomainClass {}

@available(Bering)
public struct DomainValue {
  public var value: Int
}
