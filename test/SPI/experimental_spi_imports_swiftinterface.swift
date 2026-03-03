// RUN: %target-swift-frontend -typecheck %s -swift-version 5 \
// RUN:   -experimental-spi-imports 2>&1 | %FileCheck %s -check-prefix=CHECK-5

// RUN: not %target-swift-frontend -typecheck %s -swift-version 6 \
// RUN:   -experimental-spi-imports 2>&1 | %FileCheck %s -check-prefix=CHECK-6

/// The flag is deprecated before Swift 6.
// CHECK-5: flag '-experimental-spi-imports' is deprecated

/// The flag is rejected in Swift 6.
// CHECK-6: flag '-experimental-spi-imports' is unsupported
