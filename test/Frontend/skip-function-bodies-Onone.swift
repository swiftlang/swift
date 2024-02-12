// RUN: %target-swift-frontend -typecheck -experimental-skip-non-inlinable-function-bodies -module-name SwiftOnoneSupport %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -experimental-skip-non-inlinable-function-bodies-without-types -module-name SwiftOnoneSupport %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -experimental-skip-all-function-bodies -module-name SwiftOnoneSupport %s 2>&1 | %FileCheck %s

// REQUIRES: optimized_stdlib

// CHECK: module 'SwiftOnoneSupport' cannot be built with any of the -experimental-skip-*-function-bodies* flags; they have been automatically disabled
