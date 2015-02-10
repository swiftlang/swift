// Check JIT mode
// RUN: %target-jit-run %s | FileCheck %s

// REQUIRES: swift_interpreter
// REQUIRES: objc_interop

import Foundation

@objc protocol Fungible: Runcible {
  func funge()
}
@objc protocol Runcible {
  func runce()
}

class C: Fungible {
  @objc func runce() {}
  @objc func funge() {}
}

class D {}

extension D: Fungible {
  @objc func runce() {}
  @objc func funge() {}
}

extension NSString: Fungible {
  func runce() {}
  func funge() {}
}

func check(x: AnyObject) {
  println("\(x is Fungible) \(x is Runcible)")
}

check(NSString()) // CHECK: true true
check(C()) // CHECK: true true
check(D()) // CHECK: true true
