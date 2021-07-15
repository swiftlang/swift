// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s -dump-input fail

// XFAIL: asserts
// depends on distributed actors initializers not crashing the SIL verifier

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor DA_DefaultDeinit {}

// CHECK: %0 "self"
