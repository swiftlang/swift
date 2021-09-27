// REQUIRES: CPU=arm64,OS=macosx

// rdar://83576231 - link failures on arm64
// REQUIRES: rdar83576231

// Doesn't autolink compatibility library because target OS doesn't need it
// RUN: %target-swift-frontend -target arm64-apple-macosx10.14  -emit-ir -parse-stdlib %s | %FileCheck -check-prefix=NO-FORCE-LOAD %s

public func foo() {}

// NO-FORCE-LOAD-NOT: FORCE_LOAD
// NO-FORCE-LOAD-NOT: !{!"-lswiftCompatibility50"}
// NO-FORCE-LOAD-NOT: !{!"-lswiftCompatibility51"}
// NO-FORCE-LOAD-NOT: !{!"-lswiftCompatibility52"}
// NO-FORCE-LOAD-NOT: !{!"-lswiftCompatibility53"}
// NO-FORCE-LOAD-NOT: !{!"-lswiftCompatibilityDynamicReplacements"}
