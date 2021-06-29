// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s -dump-input fail

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor DA_DefaultDeinit {}

// CHECK-LABEL: sil {{.*}} @{{.*}}(@guaranteed DA_DefaultDeinit) -> @owned -> Builtin.NativeObject
// CHECK: %0 "self"
// CHECK: [[RESIGN_REF:%.*]] = function_ref xxx
