// RUN: not %target-swift-frontend -typecheck -import-objc-header %S/Inputs/invalid_bridging_header.h %s > %t.out 2>&1 
// RUN: %FileCheck %s < %t.out

// REQUIRES: objc_interop

// CHECK: 1:12: error: cannot find interface declaration for 'UndeclaredError'
// CHECK: error: failed to import bridging header
