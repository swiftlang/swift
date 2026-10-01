// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -scan-dependencies %s -module-name Client -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -module-cache-path %t/cache -o %t/enabled.json
// RUN: %FileCheck %s --check-prefix=ENABLED < %t/enabled.json
// RUN: %target-swift-frontend -scan-dependencies %s -module-name Client -cxx-interoperability-mode=default -module-cache-path %t/cache -o %t/disabled.json
// RUN: %FileCheck %s --check-prefix=DISABLED --implicit-check-not=_SwiftCxxExceptionSupport < %t/disabled.json

// RUN: %target-swift-frontend -scan-dependencies %s -module-name Client -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -Xcc -fno-exceptions -module-cache-path %t/cache-no-eh -o %t/no-eh.json
// RUN: %FileCheck %s --check-prefix=DISABLED --implicit-check-not=_SwiftCxxExceptionSupport < %t/no-eh.json
// RUN: %target-swift-frontend -scan-dependencies %s -module-name Client -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -Xcc -fignore-exceptions -module-cache-path %t/cache-ignore-eh -o %t/ignore-eh.json
// RUN: %FileCheck %s --check-prefix=DISABLED --implicit-check-not=_SwiftCxxExceptionSupport < %t/ignore-eh.json

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

// The native facade imports this helper lazily. Explicit builds must discover
// the dependency before they import the annotated function.
// ENABLED: "clang": "_SwiftCxxExceptionSupport"
// DISABLED: "mainModuleName": "Client"
