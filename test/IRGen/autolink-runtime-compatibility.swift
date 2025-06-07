// REQUIRES: OS=macosx,CPU=x86_64

// Doesn't autolink compatibility library because autolinking is disabled
// RUN: %target-swift-frontend -disable-autolinking-runtime-compatibility-dynamic-replacements -target %target-cpu-apple-macosx10.9 -disable-autolinking-runtime-compatibility -disable-autolinking-runtime-compatibility-concurrency -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=NO-FORCE-LOAD %s
// RUN: %target-swift-frontend -disable-autolinking-runtime-compatibility-dynamic-replacements -runtime-compatibility-version 5.0 -disable-autolinking-runtime-compatibility -disable-autolinking-runtime-compatibility-concurrency -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=NO-FORCE-LOAD %s

// Doesn't autolink compatibility library because runtime compatibility library is disabled
// RUN: %target-swift-frontend -runtime-compatibility-version none -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=NO-FORCE-LOAD %s

// Doesn't autolink compatibility library because target OS doesn't need it
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx99.99 -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=NO-FORCE-LOAD %s

// Only autolinks 5.1 compatibility library because target OS has 5.1
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx10.15 -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=FORCE-LOAD-51 %s

// Only autolinks concurrency compatibility library because target OS has 5.3
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx11 -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=FORCE-LOAD-53 %s

// Only autolinks concurrency compatibility library because target OS has 5.4
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx11.3 -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=FORCE-LOAD-54 %s

// Autolinks because compatibility library was explicitly asked for
// RUN: %target-swift-frontend -runtime-compatibility-version 5.0 -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=FORCE-LOAD %s
// RUN: %target-swift-frontend -runtime-compatibility-version 5.1 -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=FORCE-LOAD-51 %s
// RUN: %target-swift-frontend -runtime-compatibility-version 5.5 -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=FORCE-LOAD-55 %s
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx99.99 -runtime-compatibility-version 5.0 -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=FORCE-LOAD %s
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx99.99 -runtime-compatibility-version 5.1 -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=FORCE-LOAD-51 %s
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx99.99 -runtime-compatibility-version 5.5 -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=FORCE-LOAD-55 %s

public func foo() {}

// NO-FORCE-LOAD-NOT: FORCE_LOAD
// NO-FORCE-LOAD-NOT: !{!"-lswiftCompatibility50"}
// NO-FORCE-LOAD-NOT: !{!"-lswiftCompatibility51"}
// NO-FORCE-LOAD-NOT: !{!"-lswiftCompatibilityDynamicReplacements"}
// NO-FORCE-LOAD-NOT: !{!"-lswiftCompatibilityConcurrency"}

// FORCE-LOAD: declare {{.*}} @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// FORCE-LOAD: declare {{.*}} @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"
// FORCE-LOAD: declare {{.*}} @"_swift_FORCE_LOAD_$_swiftCompatibilityConcurrency"
// FORCE-LOAD-DAG: [[AUTOLINK_SWIFT_COMPAT:![0-9]+]] = !{!"-lswiftCompatibility50"}
// FORCE-LOAD-DAG: !{!"-lswiftCompatibility51"}
// FORCE-LOAD-DAG: !{!"-lswiftCompatibilityDynamicReplacements"}
// FORCE-LOAD-DAG: !{!"-lswiftCompatibilityConcurrency"}
// FORCE-LOAD-DAG: !llvm.linker.options = !{{{.*}}[[AUTOLINK_SWIFT_COMPAT]]{{[,}]}}

// FORCE-LOAD-51-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// FORCE-LOAD-51-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"
// FORCE-LOAD-51-DAG: declare {{.*}} @"_swift_FORCE_LOAD_$_swiftCompatibility51"
// FORCE-LOAD-51-DAG: declare {{.*}} @"_swift_FORCE_LOAD_$_swiftCompatibilityConcurrency"
// FORCE-LOAD-51-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// FORCE-LOAD-51-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"
// FORCE-LOAD-51-DAG: [[AUTOLINK_SWIFT_COMPAT:![0-9]+]] = !{!"-lswiftCompatibility51"}
// FORCE-LOAD-51-DAG: [[AUTOLINK_SWIFT_COMPAT_CONCURRENCY:![0-9]+]] = !{!"-lswiftCompatibilityConcurrency"}
// FORCE-LOAD-51-DAG: !llvm.linker.options = !{{{.*}}[[AUTOLINK_SWIFT_COMPAT]], {{.*}}[[AUTOLINK_SWIFT_COMPAT_CONCURRENCY]]{{[,}]}}
// FORCE-LOAD-51-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// FORCE-LOAD-51-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"

// FORCE-LOAD-53-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// FORCE-LOAD-53-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"
// FORCE-LOAD-53-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility51"
// FORCE-LOAD-53-DAG: declare {{.*}} @"_swift_FORCE_LOAD_$_swiftCompatibilityConcurrency"
// FORCE-LOAD-53-DAG: [[AUTOLINK_SWIFT_COMPAT_CONCURRENCY:![0-9]+]] = !{!"-lswiftCompatibilityConcurrency"}

// FORCE-LOAD-54-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// FORCE-LOAD-54-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"
// FORCE-LOAD-54-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility51"
// FORCE-LOAD-54-DAG: declare {{.*}} @"_swift_FORCE_LOAD_$_swiftCompatibilityConcurrency"
// FORCE-LOAD-54-DAG: [[AUTOLINK_SWIFT_COMPAT_CONCURRENCY:![0-9]+]] = !{!"-lswiftCompatibilityConcurrency"}

// FORCE-LOAD-55-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility51"

// FORCE-LOAD-55-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// FORCE-LOAD-55-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"
// FORCE-LOAD-55-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility51"
// FORCE-LOAD-55-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibilityConcurrency"
// FORCE-LOAD-55-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// FORCE-LOAD-55-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"
// FORCE-LOAD-55-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility51"
// FORCE-LOAD-55-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// FORCE-LOAD-55-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"
// FORCE-LOAD-55-NOT: @"_swift_FORCE_LOAD_$_swiftCompatibility51"
