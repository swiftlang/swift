// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t/def_class.swiftmodule %S/Inputs/def_class.swift
// RUN: llvm-bcanalyzer %t/def_class.swiftmodule | FileCheck %s
// RUN: %swift -emit-silgen -I=%t %s -o /dev/null

// CHECK-NOT: FALL_BACK_TO_TRANSLATION_UNIT

import def_class

var a : Empty
var b = TwoInts(1, 2)
var c : ComputedProperty
var sum = b.x + b.y + c.value

var intWrapper = ResettableIntWrapper()
var r = intWrapper as Resettable
r.reset()

class AnotherIntWrapper : SpecialResettable {
  var value : Int
  func reset() {
    value = 0
  }
  func compute() {
    value = 42
  }
}

var intWrapper2 = AnotherIntWrapper()
r = intWrapper2
r.reset()

var c = intWrapper2 as Cacheable
c.compute()
c.reset()


var p = Pair(1, 2.5)
p.first = 2
p.second = 5.0

struct Int {}

var gc = GenericCtor<Int>()
gc.doSomething()


a = StillEmpty()
r = StillEmpty()

var bp = BoolPair()
bp.bothTrue()

var rawBP : Pair<Bool, Bool>
rawBP = bp


var rev : SpecialPair<Double>
rev.first = 42
var comp : Computable = rev
