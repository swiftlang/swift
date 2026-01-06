// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.52

// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -target %target-cpu-apple-macosx10.52 -emit-sorted-sil %s -o %t.fragile.sil
// RUN: %FileCheck -check-prefixes=CHECK,CHECK-NO-EXTENSION %s < %t.fragile.sil
// RUN: %FileCheck -check-prefixes=NEGATIVE,NEGATIVE-NO-EXTENSION %s < %t.fragile.sil

// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -target %target-cpu-apple-macosx10.52 -emit-sorted-sil %s -o %t.extensions-fragile.sil -application-extension
// RUN: %FileCheck -check-prefixes=CHECK,CHECK-EXTENSION %s < %t.extensions-fragile.sil
// RUN: %FileCheck -check-prefixes=NEGATIVE,NEGATIVE-EXTENSION %s < %t.extensions-fragile.sil

// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -target %target-cpu-apple-macosx10.52 -emit-sorted-sil %s -enable-library-evolution -o %t.resilient.sil
// RUN: %FileCheck -check-prefixes=CHECK,CHECK-NO-EXTENSION %s < %t.resilient.sil
// RUN: %FileCheck -check-prefixes=NEGATIVE,NEGATIVE-NO-EXTENSION %s < %t.resilient.sil

// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -target %target-cpu-apple-macosx10.52 -emit-sorted-sil %s -enable-library-evolution -o %t.extensions-resilient.sil -application-extension
// RUN: %FileCheck -check-prefixes=CHECK,CHECK-EXTENSION %s < %t.extensions-resilient.sil
// RUN: %FileCheck -check-prefixes=NEGATIVE,NEGATIVE-EXTENSION %s < %t.extensions-resilient.sil

// This test just requires a platform with meaningful #available() checks, but
// for simplicity, it's written for macOS only.
// REQUIRES: OS=macosx

public enum E: Int {
  // For some reason, we generate strange SIL for the first case that's
  // difficult to validate. This just gets that out of the way.
  case sacrificial = -500

  case normal = -1000

  @available(macOS 10.51, *)
  case alwaysAvailable = -2000

  @available(macOS 10.55, *)
  case potentiallyUnavailable = -3000

  @available(macOSApplicationExtension 10.56, *)
  case potentiallyUnavailableForExtensions = -3001

  @available(macOS, unavailable)
  case neverAvailable = -4000

  @available(macOSApplicationExtension, unavailable)
  case neverAvailableForExtensions = -4001

  @available(macOS, obsoleted: 10.99)
  case notObsoleteYet = -5000

  @available(macOS, obsoleted: 10.51)
  case nowObsolete = -6000
}

// CHECK-LABEL: sil {{(\[serialized\] )?}}[ossa] @$s4main1EO8rawValueACSgSi_tcfC

// CHECK: integer_literal $Builtin.IntLiteral, -1000
// CHECK: cond_br {{[^,]+}}, [[normal:bb[0-9]+]]

// CHECK: integer_literal $Builtin.IntLiteral, -2000
// CHECK: cond_br {{[^,]+}}, [[alwaysAvailable:bb[0-9]+]]

// CHECK: integer_literal $Builtin.IntLiteral, -3000
// CHECK: cond_br {{[^,]+}}, [[potentiallyUnavailable:bb[0-9]+]]

// CHECK: integer_literal $Builtin.IntLiteral, -3001
// CHECK: cond_br {{[^,]+}}, [[potentiallyUnavailableForExtensions:bb[0-9]+]]

// CHECK-NO-EXTENSION: integer_literal $Builtin.IntLiteral, -4001
// CHECK-NO-EXTENSION: cond_br {{[^,]+}}, [[neverAvailableForExtensions:bb[0-9]+]]

// CHECK: integer_literal $Builtin.IntLiteral, -5000
// CHECK: cond_br {{[^,]+}}, [[notObsoleteYet:bb[0-9]+]]

// CHECK: [[notObsoleteYet]]:
// CHECK-NOT: function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// CHECK: {{enum \$E|inject_enum_addr %[0-9]+ : \$\*E}}, #E.notObsoleteYet!enumelt

// CHECK-NO-EXTENSION: [[neverAvailableForExtensions]]:
// CHECK-NO-EXTENSION-NOT: function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// CHECK-NO-EXTENSION: {{enum \$E|inject_enum_addr %[0-9]+ : \$\*E}}, #E.neverAvailableForExtensions!enumelt

// CHECK: [[potentiallyUnavailableForExtensions]]:
// CHECK-NO-EXTENSION-NOT: function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// CHECK-NO-EXTENSION: {{enum \$E|inject_enum_addr %[0-9]+ : \$\*E}}, #E.potentiallyUnavailableForExtensions!enumelt
// CHECK-EXTENSION-NEXT: extend_lifetime
// CHECK-EXTENSION-NEXT: integer_literal $Builtin.Word, 10
// CHECK-EXTENSION-NEXT: integer_literal $Builtin.Word, 56
// CHECK-EXTENSION-NEXT: integer_literal $Builtin.Word, 0
// CHECK-EXTENSION: function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// CHECK-EXTENSION: cond_br {{[^,]+}}, [[potentiallyUnavailableForExtensions_newEnough:bb[0-9]+]],

// CHECK-EXTENSION: [[potentiallyUnavailableForExtensions_newEnough]]:
// CHECK-EXTENSION: {{enum \$E|inject_enum_addr %[0-9]+ : \$\*E}}, #E.potentiallyUnavailableForExtensions!enumelt

// CHECK: [[potentiallyUnavailable]]:
// CHECK-NEXT: extend_lifetime
// CHECK-NEXT: integer_literal $Builtin.Word, 10
// CHECK-NEXT: integer_literal $Builtin.Word, 55
// CHECK-NEXT: integer_literal $Builtin.Word, 0
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
// CHECK: switch_enum {{%.*}} : $E
// CHECK-NOT: function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// CHECK: end sil function '$s4main1EO8rawValueSivg'

// NEGATIVE-LABEL: sil {{(\[serialized\] )?}}[ossa] @$s4main1EO8rawValueACSgSi_tcfC

// Should not try to match neverAvailable's raw value
// NEGATIVE-NOT: integer_literal $Builtin.IntLiteral, -4000

// When building with -application-extension, should not try to match neverAvailableForExtensions's raw value
// NEGATIVE-EXTENSION-NOT: integer_literal $Builtin.IntLiteral, -4001

// Should not try to match nowObsolete's raw value
// NEGATIVE-NOT: integer_literal $Builtin.IntLiteral, -6000

// Should not have a version check for notObsoleteYet
// NEGATIVE-NOT: integer_literal $Builtin.Word, 99

// NEGATIVE: end sil function '$s4main1EO8rawValueACSgSi_tcfC'
