// REQUIRES: executable_test
// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -Xfrontend -enable-type-layout -Xfrontend -force-struct-type-layouts -Xfrontend -enable-autolinking-runtime-compatibility-bytecode-layouts -I %t -L %t %target-cxx-lib %s -o %t/a.out
// RUN: %target-run %t/a.out --stdlib-unittest-in-process

import Foundation
import StdlibUnittest

var Tests = TestSuite("RuntimeValueWitnessObjC")


func assignCopy<T>(_ a: T, _ b: inout T) {
  b = a
}

func assignTake<T>(_ a: T, _ b: inout T) {
  let ret1: T = a
  b = ret1
}

func initCopy<T>(_ a: T) -> T {
  let ret: T = a
  return ret
}

func initTake<T>(_ a: T) -> T {
  let ret1: T = a
  let ret2: T = ret1
  return ret2
}

func checkCopies<T: Equatable>(_ a: T, _ b: inout T) {
  let savedB: T = b
  assignCopy(a, &b)
  expectEqual(a, b)

  b = savedB
  assignTake(a, &b)
  expectEqual(a, b)

  expectEqual(a, initCopy(a))
  expectEqual(a, initTake(a))
}

@objc class ClassA : NSObject {
  init(_ value: Int) {
    self.tracked = LifetimeTracked(value)
  }

  let tracked: LifetimeTracked
}

Tests.test("MultiReferenceStruct") {
  @_GenerateLayoutBytecode
  struct LifetimeStruct : Equatable {
    init(a: ClassA, b: ClassA, c: ClassA) {
      self.a = a
      self.b = b
      self.c = c
    }
    let a: ClassA
    let b: ClassA
    let c: ClassA
  }
  let a = LifetimeStruct(a: ClassA(0), b: ClassA(0), c: ClassA(0))
  var b = LifetimeStruct(a: ClassA(1), b: ClassA(1), c: ClassA(1))
  checkCopies(a, &b)
}


@_GenerateLayoutBytecode
struct BlockStruct : Equatable {
  init(a: @escaping (Int) -> (), b: ClassA) {
    self.a = a
    self.b = b
  }

  static func == (lhs: BlockStruct, rhs: BlockStruct) -> Bool {
    return true
  }

  let a: @convention(block) (Int) -> ()
  let b: ClassA
}

Tests.test("Blocks") {
  let lt1 = LifetimeTracked(0)
  let lt2 = LifetimeTracked(1)
  let a = BlockStruct(a: { _ in print(lt1) }, b: ClassA(0))
  var b = BlockStruct(a: { _ in print(lt2) }, b: ClassA(1))
  checkCopies(a, &b)

}

runAllTests()
