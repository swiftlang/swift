// RUN: env SWIFT_FORCE_MODULE_LOADING=garbage not %target-swift-frontend -typecheck %s 2>&1 | %FileCheck %s

// CHECK: error: unknown value for SWIFT_FORCE_MODULE_LOADING variable: 'garbage'
