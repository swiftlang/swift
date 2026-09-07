// Build the same zippered library twice. macOS clients load the build that has
// macOS as its primary target, and macCatalyst clients load the build that has
// macCatalyst as its primary target. Both builds emit the same availability
// query entry points.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/macos)
// RUN: %empty-directory(%t/maccatalyst)

// RUN: %target-swift-frontend -O -module-name ZipperedLib -emit-module -emit-module-path %t/macos/ZipperedLib.swiftmodule -target %target-cpu-apple-macosx10.15 -target-variant %target-cpu-apple-ios13.1-macabi %S/Inputs/constant_propagation_availability_zippered_lib.swift
// RUN: %target-swift-frontend -O -module-name ZipperedLib -emit-module -emit-module-path %t/maccatalyst/ZipperedLib.swiftmodule -target %target-cpu-apple-ios13.1-macabi -target-variant %target-cpu-apple-macosx10.15 %S/Inputs/constant_propagation_availability_zippered_lib.swift

// Every client below deploys to macOS 10.53, to macCatalyst 51.0, or to both.
// The library queries versions on either side of those two, so the queried
// version decides whether a query folds.

// A zippered client deploys to both platforms.
// RUN: %target-swift-frontend -O -emit-sil -module-name Client -I %t/macos %s -target %target-cpu-apple-macosx10.53 -target-variant %target-cpu-apple-ios51.0-macabi | %FileCheck %s --check-prefixes=CHECK,DEPLOYS-MACOS,DEPLOYS-MACCATALYST

// A macOS only client deploys to macOS alone.
// RUN: %target-swift-frontend -O -emit-sil -module-name Client -I %t/macos %s -target %target-cpu-apple-macosx10.53 | %FileCheck %s --check-prefixes=CHECK,DEPLOYS-MACOS,MACOS-ONLY

// A macCatalyst only client deploys to macCatalyst alone.
// RUN: %target-swift-frontend -O -emit-sil -module-name Client -I %t/maccatalyst %s -target %target-cpu-apple-ios51.0-macabi | %FileCheck %s --check-prefixes=CHECK,DEPLOYS-MACCATALYST,MACCATALYST-ONLY

// REQUIRES: OS=macosx || OS=maccatalyst
// REQUIRES: maccatalyst_support

import ZipperedLib

// CHECK-LABEL:           sil{{.*}}@$s6Client13testBothBelowSiyF :
// CHECK-NOT:               OSVersionAtLeast
// CHECK-NOT:               cond_br
// CHECK:                   function_ref @$s11ZipperedLib11newerOnBothSiyF
// CHECK-NOT:               OSVersionAtLeast
// CHECK-NOT:               cond_br
// CHECK:                 } // end sil function '$s6Client13testBothBelowSiyF'
public func testBothBelow() -> Int {
  return queryBothBelow()
}

// CHECK-LABEL:           sil{{.*}}@$s6Client18testBothAboveMacOSSiyF :
// MACCATALYST-ONLY-NOT:    OSVersionAtLeast
// MACCATALYST-ONLY-NOT:    cond_br
// DEPLOYS-MACOS:           function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF
// DEPLOYS-MACOS:           cond_br
// CHECK:                   function_ref @$s11ZipperedLib11newerOnBothSiyF
// MACCATALYST-ONLY-NOT:    OSVersionAtLeast
// MACCATALYST-ONLY-NOT:    cond_br
// CHECK:                 } // end sil function '$s6Client18testBothAboveMacOSSiyF'
public func testBothAboveMacOS() -> Int {
  return queryBothAboveMacOS()
}

// CHECK-LABEL:           sil{{.*}}@$s6Client24testBothAboveMacCatalystSiyF :
// MACOS-ONLY-NOT:          OSVersionAtLeast
// MACOS-ONLY-NOT:          cond_br
// DEPLOYS-MACCATALYST:     function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF
// DEPLOYS-MACCATALYST:     cond_br
// CHECK:                   function_ref @$s11ZipperedLib11newerOnBothSiyF
// MACOS-ONLY-NOT:          OSVersionAtLeast
// MACOS-ONLY-NOT:          cond_br
// CHECK:                 } // end sil function '$s6Client24testBothAboveMacCatalystSiyF'
public func testBothAboveMacCatalyst() -> Int {
  return queryBothAboveMacCatalyst()
}

// CHECK-LABEL:           sil{{.*}}@$s6Client14testMacOSBelowSiyF :
// DEPLOYS-MACOS-NOT:       OSVersionAtLeast
// DEPLOYS-MACOS-NOT:       cond_br
// MACCATALYST-ONLY:        builtin "targetOSVersionAtLeast"
// MACCATALYST-ONLY:        cond_br
// CHECK:                   function_ref @$s11ZipperedLib12newerOnMacOSSiyF
// DEPLOYS-MACOS-NOT:       OSVersionAtLeast
// DEPLOYS-MACOS-NOT:       cond_br
// CHECK:                 } // end sil function '$s6Client14testMacOSBelowSiyF'
public func testMacOSBelow() -> Int {
  return queryMacOSBelow()
}

// CHECK-LABEL:           sil{{.*}}@$s6Client14testMacOSAboveSiyF :
// DEPLOYS-MACOS:           function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// MACCATALYST-ONLY:        builtin "targetOSVersionAtLeast"
// CHECK:                   cond_br
// CHECK:                   function_ref @$s11ZipperedLib12newerOnMacOSSiyF
// CHECK:                 } // end sil function '$s6Client14testMacOSAboveSiyF'
public func testMacOSAbove() -> Int {
  return queryMacOSAbove()
}

// CHECK-LABEL:           sil{{.*}}@$s6Client20testMacCatalystBelowSiyF :
// DEPLOYS-MACCATALYST-NOT: OSVersionAtLeast
// DEPLOYS-MACCATALYST-NOT: cond_br
// MACOS-ONLY:              function_ref @$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF
// MACOS-ONLY:              cond_br
// CHECK:                   function_ref @$s11ZipperedLib18newerOnMacCatalystSiyF
// DEPLOYS-MACCATALYST-NOT: OSVersionAtLeast
// DEPLOYS-MACCATALYST-NOT: cond_br
// CHECK:                 } // end sil function '$s6Client20testMacCatalystBelowSiyF'
public func testMacCatalystBelow() -> Int {
  return queryMacCatalystBelow()
}

// CHECK-LABEL:           sil{{.*}}@$s6Client20testMacCatalystAboveSiyF :
// CHECK:                   function_ref @$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF
// CHECK:                   cond_br
// CHECK:                   function_ref @$s11ZipperedLib18newerOnMacCatalystSiyF
// CHECK:                 } // end sil function '$s6Client20testMacCatalystAboveSiyF'
public func testMacCatalystAbove() -> Int {
  return queryMacCatalystAbove()
}
