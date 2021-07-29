// RUN: %target-swift-frontend -enable-experimental-distributed -disable-availability-checking -emit-ir -parse-stdlib %s | %FileCheck %s --dump-input=always

// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed


@available(SwiftStdlib 5.5, *)
distributed actor FancyWorker {}

// CHECK: kappa