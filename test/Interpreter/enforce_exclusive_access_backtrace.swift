// RUN: %empty-directory(%t)
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

func writeAndPerform<T>(_ _: UnsafeMutablePointer<T>, closure: () ->()) {
  closure()
}

var globalX = X()
withUnsafePointer(to: &globalX) { _ = fputs(String(format: "globalX: 0x%lx\n", Int(bitPattern: $0)), stderr) }
// CHECK: globalX: [[ADDR:0x.*]]

fputs("Global Access\n", stderr);
readAndPerform(&globalX) {
  globalX = X()
  // CHECK-LABEL: Global Access
  // CHECK: Simultaneous accesses to [[ADDR]], but modification requires exclusive access.
  // CHECK: Previous access (a read) started at a.out`main + {{.*}} (0x{{.*}}).
  // CHECK: Current access (a modification) started at:
  // CHECK: a.out {{.*}} closure
  // CHECK: a.out {{.*}} readAndPerform
  // CHECK: a.out {{.*}} main
}

class C {
  final var f = 7
}

var c = C()
withUnsafePointer(to: &c.f) { _ = fputs(String(format: "c.f: 0x%lx\n", Int(bitPattern: $0)), stderr) }
// CHECK: c.f: [[C_F_ADDR:0x.*]]

// Key path accesses are performed in the Standard Library. The Standard Library
// is currently compiled in Swift 3 mode and the compiler currently logs conflicts
// (rather than trapping on them) code compiled in Swift 3 mode. For this reason
// conflicts where the second conflicting access is via a key path will log rather than
// trap.

fputs("Overlapping Key Path Write Accesses\n", stderr);
writeAndPerform(&c[keyPath: \C.f]) {
  c[keyPath: \C.f] = 8
  // CHECK-LABEL: Overlapping Key Path Write Accesses
  // CHECK: Simultaneous accesses to [[C_F_ADDR]], but modification requires exclusive access.
  // CHECK: Previous access (a modification)
  // CHECK: Current access (a modification)
  // CHECK: a.out {{.*}} closure
  // CHECK: a.out {{.*}} writeAndPerform
  // CHECK: a.out {{.*}} main
}

fputs("Overlapping Key Path Write Access and Key Path Read Access\n", stderr);
writeAndPerform(&c[keyPath: \C.f]) {
  let x = c[keyPath: \C.f]
  _blackHole(x)
  // CHECK-LABEL: Overlapping Key Path Write Access and Key Path Read Access
  // CHECK: Simultaneous accesses to [[C_F_ADDR]], but modification requires exclusive access.
  // CHECK: Previous access (a modification)
  // CHECK: Current access (a read)
  // CHECK: a.out {{.*}} closure
  // CHECK: a.out {{.*}} writeAndPerform
  // CHECK: a.out {{.*}} main
}
