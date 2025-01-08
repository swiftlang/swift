// RUN: %target-swift-frontend -typecheck -enable-experimental-feature WarnUnsafe -Ounchecked -disable-access-control %s 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_WarnUnsafe

// CHECK: warning: '-Ounchecked' is not memory-safe
// CHECK: warning: '-disable-access-control' is not memory-safe
