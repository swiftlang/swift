// Check JIT mode
// RUN: %swift -sdk %sdk -interpret %s | FileCheck %s

import Foundation

@objc protocol Fungible: Runcible {
  func funge()
}
@objc protocol Runcible {
  func runce()
}

class C: Fungible {
  func runce() {}
  func funge() {}
}

class D {}

extension D: Fungible {
  func runce() {}
  func funge() {}
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
