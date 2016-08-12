// Check JIT mode
// RUN: %target-jit-run %s | %FileCheck %s

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

func check(_ x: AnyObject) {
  print("\(x is Fungible) \(x is Runcible)")
}

check(NSString()) // CHECK: true true
check(C()) // CHECK: true true
check(D()) // CHECK: true true

// Make sure partial application of methods with @autoreleased
// return values works

var count = 0

class Juice : NSObject {
  override init() {
    count += 1
  }

  deinit {
    count -= 1
  }
}

@objc protocol Fruit {
  @objc optional var juice: Juice { get }
}

class Durian : NSObject, Fruit {
  init(juice: Juice) {
    self.juice = juice
  }

  var juice: Juice
}

func consume(_ fruit: Fruit) {
  _ = fruit.juice
}

autoreleasepool {
  let tasty = Durian(juice: Juice())
  print(count) // CHECK: 1
  consume(tasty)
}

do {
  print(count) // CHECK: 0
}
