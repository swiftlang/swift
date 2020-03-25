// REQUIRES: CPU=arm64e,OS=ios

// Doesn't autolink compatibility library because target OS doesn't need it
// RUN: %target-swift-frontend -target arm64e-apple-ios11.0  -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=NO-FORCE-LOAD %s

public func foo() {}

// NO-FORCE-LOAD-NOT: FORCE_LOAD
// NO-FORCE-LOAD-NOT: !{!"-lswiftCompatibility50"}
// NO-FORCE-LOAD-NOT: !{!"-lswiftCompatibilityDynamicReplacements"}
