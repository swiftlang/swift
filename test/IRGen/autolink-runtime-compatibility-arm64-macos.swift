// REQUIRES: CPU=arm64,OS=macosx

// The baseline arm64 macOS deployment target (Swift 5.3 / macOS 10.16) doesn't
// need the pre-5.5 compatibility libraries, but the concurrency-era ones must
// still be back-deployed.
// RUN: %target-swift-frontend -target arm64-apple-macosx10.14  -emit-ir -parse-stdlib %s | %FileCheck %s

public func foo() {}

// The pre-5.5 compatibility libraries are not force-loaded...
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility51"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility52"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility53"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"
// ...but the concurrency-era ones are.
// CHECK-DAG: @"_swift_FORCE_LOAD_$_swiftCompatibilityConcurrency"
// CHECK-DAG: @"_swift_FORCE_LOAD_$_swiftCompatibility56"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility51"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility52"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility53"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"

// Likewise for the autolink metadata.
// CHECK-NOT: !{!"-lswiftCompatibility50"}
// CHECK-NOT: !{!"-lswiftCompatibility51"}
// CHECK-NOT: !{!"-lswiftCompatibility52"}
// CHECK-NOT: !{!"-lswiftCompatibility53"}
// CHECK-NOT: !{!"-lswiftCompatibilityDynamicReplacements"}
// CHECK-DAG: !{!"-lswiftCompatibilityConcurrency"}
// CHECK-DAG: !{!"-lswiftCompatibility56"}
