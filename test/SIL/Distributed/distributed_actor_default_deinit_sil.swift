// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed actor SimpleEmptyDistributedActor {
}

// CHECK: // SimpleEmptyDistributedActor.deinit
// CHECK: TEST