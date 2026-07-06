// RUN: %empty-directory(%t)

// RUN: env SWIFT_INDEX_STORE_PATH=%t/idx-no-options %target-swift-frontend -typecheck %s
// RUN: c-index-test core -print-record %t/idx-no-options | %FileCheck %s --check-prefix=NO_OPTIONS

// RUN: env SWIFT_INDEX_STORE_PATH=%t/idx-with-options SWIFT_INDEX_STORE_OPTIONS="-index-include-locals" %target-swift-frontend -typecheck %s
// RUN: c-index-test core -print-record %t/idx-with-options | %FileCheck %s --check-prefix=WITH_OPTIONS

// RUN: env SWIFT_INDEX_STORE_PATH=%t/idx-with-multiple-options SWIFT_INDEX_STORE_OPTIONS="-index-include-locals -index-system-modules" %target-swift-frontend -typecheck %s
// RUN: c-index-test core -print-record %t/idx-with-multiple-options | %FileCheck %s --check-prefix=MULTIPLE_OPTIONS

// NO_OPTIONS: [[@LINE+3]]:13 | function(public)/Swift
// WITH_OPTIONS: [[@LINE+2]]:13 | function/Swift
// MULTIPLE_OPTIONS: [[@LINE+1]]:13 | function/Swift
public func foo() {
  // NO_OPTIONS-NOT: localVar
  // WITH_OPTIONS: [[@LINE+2]]:7 | variable(local)/Swift
  // MULTIPLE_OPTIONS: [[@LINE+1]]:7 | variable(local)/Swift
  let localVar = 2
}

// NO_OPTIONS-NOT: struct/Swift | String
// WITH_OPTIONS-NOT: struct/Swift | String
// MULTIPLE_OPTIONS: struct/Swift | String
