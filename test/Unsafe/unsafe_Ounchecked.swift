// RUN: %target-swift-frontend -typecheck -enable-experimental-feature WarnUnsafe -Ounchecked %s 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_WarnUnsafe

// CHECK: warning: '-Ounchecked' is not memory-safe
