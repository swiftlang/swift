// REQUIRES: VENDOR=apple 
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -application-extension -emit-tbd -emit-tbd-path %t/safe.tbd
// RUN: %target-swift-frontend -typecheck %s -emit-tbd -emit-tbd-path %t/not-safe.tbd

// RUN: %FileCheck %s --check-prefix EXTENSIONSAFE < %t/safe.tbd
// RUN: %FileCheck %s --check-prefix NOTEXTENSIONSAFE < %t/not-safe.tbd

// EXTENSIONSAFE-NOT: not_app_extension_safe
// NOTEXTENSIONSAFE: not_app_extension_safe
