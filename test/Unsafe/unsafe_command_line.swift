// RUN: %target-swift-frontend -typecheck -strict-memory-safety -Ounchecked -disable-access-control %s 2>&1 | %FileCheck %s

// CHECK: warning: '-Ounchecked' is not memory-safe
// CHECK: warning: '-disable-access-control' is not memory-safe
