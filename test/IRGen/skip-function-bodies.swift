// RUN: not %target-swift-frontend -emit-ir %s -experimental-skip-non-inlinable-function-bodies %s 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -c %s -experimental-skip-non-inlinable-function-bodies %s 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -emit-ir %s -experimental-skip-non-inlinable-function-bodies-without-types %s 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -c %s -experimental-skip-non-inlinable-function-bodies-without-types %s 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -emit-ir %s -experimental-skip-all-function-bodies %s 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -c %s -experimental-skip-all-function-bodies %s 2>&1 | %FileCheck %s

// CHECK: the -experimental-skip-*-function-bodies* flags do not support emitting IR
