// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift -swift-version 3 %s -o %t/a.out -enforce-exclusivity=checked -Onone
//
// RUN: %target-run %t/a.out 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import StdlibUnittest
import SwiftPrivatePthreadExtras
import Foundation

struct X {
  var i = 7
}

func readAndPerform<T>(_ _: UnsafePointer<T>, closure: () ->()) {
  closure()
}

var globalX = X()
withUnsafePointer(to: &globalX) { _ = fputs(String(format: "globalX: 0x%lx\n", Int(bitPattern: $0)), stderr) }
// CHECK: globalX: [[ADDR:0x.*]]

readAndPerform(&globalX) {
  globalX = X()
  // CHECK: Simultaneous accesses to [[ADDR]], but modification requires exclusive access.
  // CHECK: Previous access (a read) started at a.out`main + {{.*}} (0x{{.*}}).
  // CHECK: Current access (a modification) started at:
  // CHECK: a.out {{.*}} closure
  // CHECK: a.out {{.*}} readAndPerform
  // CHECK: a.out {{.*}} main
}
