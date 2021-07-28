// REQUIRES: CPU=arm64,OS=macosx

// Only autolink compatibility libraries before 5.5 since target OS doesn't need
// it.

// RUN: %target-swift-frontend -target arm64-apple-macosx10.14  -emit-ir -parse-stdlib %s | %FileCheck %s

public func foo() {}

// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility51"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility52"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility53"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"
// CHECK-DAG: declare {{.*}} @"_swift_FORCE_LOAD_$_swiftCompatibilityConcurrency"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility51"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility52"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility53"
// CHECK-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"

// CHECK-NOT: !{!"-lswiftCompatibility50"}
// CHECK-NOT: !{!"-lswiftCompatibility51"}
// CHECK-NOT: !{!"-lswiftCompatibility52"}
// CHECK-NOT: !{!"-lswiftCompatibility53"}
// CHECK-NOT: !{!"-lswiftCompatibilityDynamicReplacements"}
// CHECK-DAG: !{!"-lswiftCompatibilityConcurrency"}
