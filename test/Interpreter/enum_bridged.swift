// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop
import Foundation
class K {}
enum MixedBridged {
  case Native(K)
  case Bridged([K])
  case Empty
}

struct Container {
  var storage : MixedBridged = .Empty

  mutating func adoptStyle(_ s: [K]) {
    storage = .Bridged(s)
  }
}

func copyStorage(_ s: [K], _ x : Container) -> Container {
  var c = x
  c.adoptStyle(s)
  return c
}

func testCase() {
  let nsArray = NSArray(objects:K(), K())  as! [K]
  let l = Container()
  let c = copyStorage(nsArray, l)
  print(c)
}

// CHECK: Container(storage: a.MixedBridged.Bridged([a.K, a.K]))
testCase()
