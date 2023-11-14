// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -application-extension -emit-tbd -emit-tbd-path %t/safe.tbd -tbd-install_name app_safe
// RUN: %target-swift-frontend -typecheck %s -emit-tbd -emit-tbd-path %t/not-safe.tbd -tbd-install_name no_app_safe

// RUN: %validate-json %t/safe.tbd | %FileCheck %s --check-prefix EXTENSIONSAFE
// RUN: %validate-json %t/not-safe.tbd | %FileCheck %s --check-prefix NOTEXTENSIONSAFE

// EXTENSIONSAFE-NOT: not_app_extension_safe
// NOTEXTENSIONSAFE: not_app_extension_safe

// RUN: %target-swift-frontend -target-variant %target-cpu-apple-ios13.1-macabi -typecheck %s -application-extension -emit-tbd -emit-tbd-path %t/target-variant.tbd -tbd-install_name target-variant
// RUN: %validate-json %t/target-variant.tbd | %FileCheck %s --check-prefix MACABI

// MACABI: "target": "{{.*}}-macos"
// MACABI: "target": "{{.*}}-maccatalyst"
