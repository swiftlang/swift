// REQUIRES: OS=macosx

// Doesn't autolink compatibility library because autolinking is disabled
// RUN: %target-swift-frontend -disable-autolinking-runtime-compatibility-dynamic-replacements -target x86_64-apple-macosx10.9 -disable-autolinking-runtime-compatibility -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=NO-FORCE-LOAD %s
// RUN: %target-swift-frontend -disable-autolinking-runtime-compatibility-dynamic-replacements -runtime-compatibility-version 5.0 -disable-autolinking-runtime-compatibility -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=NO-FORCE-LOAD %s

// Doesn't autolink compatibility library because runtime compatibility library is disabled
// RUN: %target-swift-frontend -runtime-compatibility-version none -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=NO-FORCE-LOAD %s

// Doesn't autolink compatibility library because target OS doesn't need it
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.24 -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=NO-FORCE-LOAD %s

// Autolinks because compatibility library was explicitly asked for
// RUN: %target-swift-frontend -runtime-compatibility-version 5.0 -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=FORCE-LOAD %s
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.24 -runtime-compatibility-version 5.0 -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=FORCE-LOAD %s

public func foo() {}

// NO-FORCE-LOAD-NOT: FORCE_LOAD
// NO-FORCE-LOAD-NOT: !{!"-lswiftCompatibility50"}
// NO-FORCE-LOAD-NOT: !{!"-lswiftCompatibilityDynamicReplacements"}

// FORCE-LOAD: declare {{.*}} @"_swift_FORCE_LOAD_$_swiftCompatibility50"
// FORCE-LOAD: declare {{.*}} @"_swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements"

// FORCE-LOAD-DAG: [[AUTOLINK_SWIFT_COMPAT:![0-9]+]] = !{!"-lswiftCompatibility50"}
// FORCE-LOAD-DAG: !{!"-lswiftCompatibilityDynamicReplacements"}
// FORCE-LOAD-DAG: !llvm.linker.options = !{{{.*}}[[AUTOLINK_SWIFT_COMPAT]]{{[,}]}}
