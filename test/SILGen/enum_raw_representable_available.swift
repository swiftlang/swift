// RUN: %target-typecheck-verify-swift

// RUN: %target-swift-emit-silgen -emit-sorted-sil -o %t.fragile.sil %s
// RUN: %FileCheck %s < %t.fragile.sil
// RUN: %FileCheck -check-prefix NEGATIVE %s < %t.fragile.sil

// RUN: %target-swift-emit-silgen -emit-sorted-sil -enable-resilience -o %t.resilient.sil %s
// RUN: %FileCheck %s < %t.resilient.sil
// RUN: %FileCheck -check-prefix NEGATIVE %s < %t.resilient.sil

// This test just requires a platform with meaningful #available() checks, but
// for simplicity, it's written for macOS only.
// REQUIRES: OS=macosx

public enum E: Int {
  // For some reason, we generate strange SIL for the first case that's
  // difficult to validate. This just gets that out of the way.
  case sacrificial = -500

  case normal = -1000

  @available(macOS 10.8, *)
  case alwaysAvailable = -2000

  @available(macOS 500.600.700, *)
  case potentiallyUnavailable = -3000

  @available(macOS, unavailable)
  case neverAvailable = -4000

}

// CHECK-LABEL: sil {{(\[serialized\] )?}}[ossa] @$s4main1EO8rawValueACSgSi_tcfC

// CHECK: integer_literal $Builtin.IntLiteral, -1000
// CHECK: cond_br {{[^,]+}}, [[normal:bb[0-9]+]]

// CHECK: integer_literal $Builtin.IntLiteral, -2000
// CHECK: cond_br {{[^,]+}}, [[alwaysAvailable:bb[0-9]+]]

// CHECK: integer_literal $Builtin.IntLiteral, -3000
// CHECK: cond_br {{[^,]+}}, [[potentiallyUnavailable:bb[0-9]+]]

// CHECK: [[potentiallyUnavailable]]:
// CHECK-NEXT: integer_literal $Builtin.Word, 500
// CHECK-NEXT: integer_literal $Builtin.Word, 600
// CHECK-NEXT: integer_literal $Builtin.Word, 700
// CHECK: function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// CHECK: cond_br {{[^,]+}}, [[potentiallyUnavailable_newEnough:bb[0-9]+]],

// CHECK: [[potentiallyUnavailable_newEnough]]:
// CHECK: {{enum \$E|inject_enum_addr %[0-9]+ : \$\*E}}, #E.potentiallyUnavailable!enumelt

// CHECK: [[alwaysAvailable]]:
// CHECK-NOT: function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// CHECK: {{enum \$E|inject_enum_addr %[0-9]+ : \$\*E}}, #E.alwaysAvailable!enumelt

// CHECK: [[normal]]:
// CHECK-NOT: function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// CHECK: {{enum \$E|inject_enum_addr %[0-9]+ : \$\*E}}, #E.normal!enumelt

// CHECK: end sil function '$s4main1EO8rawValueACSgSi_tcfC'

// CHECK-LABEL: sil {{(\[serialized\] )?}}[ossa] @$s4main1EO8rawValueSivg
// CHECK: {{switch_enum %0 : \$E|switch_enum_addr %2 : \$\*E}}
// CHECK-NOT: function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// CHECK: end sil function '$s4main1EO8rawValueSivg'

// NEGATIVE-LABEL: sil {{(\[serialized\] )?}}[ossa] @$s4main1EO8rawValueACSgSi_tcfC
// NEGATIVE-NOT: integer_literal $Builtin.IntLiteral, -4000
// NEGATIVE: end sil function '$s4main1EO8rawValueACSgSi_tcfC'
