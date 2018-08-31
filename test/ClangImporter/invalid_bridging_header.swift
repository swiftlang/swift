// RUN: not %target-swift-frontend -typecheck -enable-objc-interop -import-objc-header %S/Inputs/invalid_bridging_header.h %s -o - 2>&1 | %FileCheck %s

// CHECK: 1:12: error: cannot find interface declaration for 'UndeclaredError'
// CHECK: error: failed to import bridging header
