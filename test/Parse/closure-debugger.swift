// RUN: not %target-swift-frontend %s -typecheck -debugger-support 2>&1 | %FileCheck %s --check-prefix=DEBUG
// RUN: not %target-swift-frontend %s -typecheck 2>&1 | %FileCheck %s --check-prefix=NODEBUG

// DEBUG: error: cannot find '$0' in scope
// NODEBUG: error: anonymous closure argument not contained in a closure
$0
