// DEFINE: %{args} = \
// DEFINE:   -module-name main \
// DEFINE:   -enable-experimental-feature CustomAvailability \
// DEFINE:   -define-enabled-availability-domain EnabledDomain \
// DEFINE:   -define-disabled-availability-domain DisabledDomain \
// DEFINE:   -define-dynamic-availability-domain DynamicDomain

// RUN: %target-swift-emit-sil %s -verify -Onone %{args} > %t.sil
// RUN: %FileCheck %s < %t.sil
// RUN: %FileCheck %s --check-prefix=NEGATIVE < %t.sil

// REQUIRES: swift_feature_CustomAvailability

// The derived members of a raw representable enum must not mention a case that
// can never be reached at runtime, even when optimizations are disabled.
// Otherwise the raw values of the cases that a disabled domain hides would be
// recoverable from the binary. No case below shares a name with another as a
// substring, so that each check below matches exactly one case.

public enum E: String {
  case alwaysAvailable

  @available(EnabledDomain)
  case introducedByEnabled

  @available(EnabledDomain, unavailable)
  case hiddenByEnabled

  @available(DisabledDomain)
  case introducedByDisabled

  @available(DisabledDomain, unavailable)
  case hiddenByDisabled

  @available(DynamicDomain)
  case introducedByDynamic

  @available(DynamicDomain, unavailable)
  case hiddenByDynamic
}

// The reachable cases keep their raw values. A case that a dynamic domain
// restricts is reachable, because the domain may be enabled at runtime.

// CHECK-DAG: string_literal utf8 "alwaysAvailable"
// CHECK-DAG: string_literal utf8 "introducedByEnabled"
// CHECK-DAG: string_literal utf8 "hiddenByDisabled"
// CHECK-DAG: string_literal utf8 "introducedByDynamic"
// CHECK-DAG: string_literal utf8 "hiddenByDynamic"

// A case that an enabled domain marks unavailable, and a case that only a
// disabled domain introduces, can never be reached. Neither the raw value nor a
// reference to the case may appear in any function. The label below skips the
// declaration listing that precedes the functions, which names every case.

// NEGATIVE-LABEL: sil @main :
// NEGATIVE-NOT: hiddenByEnabled
// NEGATIVE-NOT: introducedByDisabled
