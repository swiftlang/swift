// RUN: %swift -typecheck %s -target x86_64-apple-macosx13.0 -target-cpu haswell -enable-experimental-feature TargetFeaturePredicate -parse-stdlib 2>&1 | %FileCheck -check-prefix CHECK-HASWELL %s
// RUN: %swift -typecheck %s -target x86_64-apple-macosx13.0 -target-cpu core2 -enable-experimental-feature TargetFeaturePredicate -parse-stdlib 2>&1 | %FileCheck -check-prefix CHECK-CORE2 %s
// RUN: %swift -typecheck %s -target arm64-apple-macosx13.0 -target-cpu apple-a14 -enable-experimental-feature TargetFeaturePredicate -parse-stdlib 2>&1 | %FileCheck -check-prefix CHECK-A14 %s
// RUN: %swift -typecheck %s -target arm64-apple-macosx13.0 -target-cpu generic -enable-experimental-feature TargetFeaturePredicate -parse-stdlib 2>&1 | %FileCheck -check-prefix CHECK-GENERIC-ARM %s
// RUN: %swift -typecheck %s -target x86_64-apple-macosx13.0 -target-cpu core2 -enable-experimental-feature TargetFeaturePredicate -parse-stdlib -Xcc -Xclang -Xcc -target-feature -Xcc -Xclang -Xcc +avx2 2>&1 | %FileCheck -check-prefix CHECK-CORE2-XCC-OVERRIDE %s

// REQUIRES: swift_feature_TargetFeaturePredicate

#if _hasTargetFeature("avx2")
#warning("has avx2")
#else
#warning("no avx2")
#endif

// A misspelled/unknown feature name is not a real feature
#if _hasTargetFeature("avx3000")
#warning("has avx3000")
#else
#warning("no avx3000")
#endif

#if arch(aarch64) && _hasTargetFeature("dotprod")
#warning("has dotprod")
#else
#warning("no dotprod")
#endif

// CHECK-HASWELL: has avx2
// CHECK-HASWELL: no avx3000
// CHECK-HASWELL: no dotprod

// CHECK-CORE2: no avx2
// CHECK-CORE2: no avx3000
// CHECK-CORE2: no dotprod

// CHECK-A14: no avx2
// CHECK-A14: no avx3000
// CHECK-A14: has dotprod

// CHECK-GENERIC-ARM: no avx2
// CHECK-GENERIC-ARM: no avx3000
// CHECK-GENERIC-ARM: no dotprod

// CHECK-CORE2-XCC-OVERRIDE: has avx2
// CHECK-CORE2-XCC-OVERRIDE: no avx3000
// CHECK-CORE2-XCC-OVERRIDE: no dotprod
