// RUN: not env SWIFT_FORCE_MODULE_LOADING=garbage %target-swift-frontend -typecheck %s 2>&1 | %FileCheck %s

// CHECK: error: unknown value for SWIFT_FORCE_MODULE_LOADING variable: 'garbage'
