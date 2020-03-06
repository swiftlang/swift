// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -application-extension -emit-tbd -emit-tbd-path %t/safe.tbd
// RUN: %target-swift-frontend -typecheck %s -emit-tbd -emit-tbd-path %t/not-safe.tbd

// RUN: %FileCheck %s --check-prefix EXTENSIONSAFE < %t/safe.tbd
// RUN: %FileCheck %s --check-prefix NOTEXTENSIONSAFE < %t/not-safe.tbd

// EXTENSIONSAFE-NOT: not_app_extension_safe
// NOTEXTENSIONSAFE: not_app_extension_safe

// RUN: %target-swift-frontend -target-variant x86_64-apple-ios13.0-macabi -typecheck %s -application-extension -emit-tbd -emit-tbd-path %t/target-variant.tbd
// RUN: %FileCheck %s --check-prefix ZIPPERED < %t/target-variant.tbd

// ZIPPERED: platform: zippered
