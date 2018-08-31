// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_struct.swift
// RUN: llvm-bcanalyzer %t/def_struct.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -I %t %s -o /dev/null

// CHECK-NOT: UnknownCode

import def_struct

var a : Empty
var b = TwoInts(x: 1, y: 2)
var c : ComputedProperty
var sum = b.x + b.y + c.value

var intWrapper = ResettableIntWrapper(value: b.x)
var r : Resettable = intWrapper
r.reset()

struct AnotherIntWrapper : SpecialResettable {
  var value : Int
  mutating
  func reset() {
    value = 0
  }
  mutating
  func compute() {
    value = 42
  }
}

var intWrapper2 = AnotherIntWrapper(value: 42)
r = intWrapper2
r.reset()

var cached : Cacheable = intWrapper2
cached.compute()
cached.reset()


var p = Pair(a: 1, b: 2.5)
p.first = 2
p.second = 5.0

var gc = GenericCtor<Int>(42)
gc.doSomething(42)

var wrappedTypeVar : ComputableWrapper<AnotherIntWrapper>.ComputableType
wrappedTypeVar = intWrapper2

cacheViaWrappers(ComputableWrapper<AnotherIntWrapper>(),
                 ResettableWrapper<AnotherIntWrapper>())


var simpleSub = ReadonlySimpleSubscript()
var subVal = simpleSub[4]

var complexSub = ComplexSubscript()
complexSub[4, false] = complexSub[3, true]

a.doAbsolutelyNothing()
var comp : Computable = UnComputable(x: 42)
var condition = UnComputable.canCompute()
var revP = p.swap()


func testMasterConformanceMap(_ x: Int32) -> Bool {
  return x != -1
}

struct TestStaticProperties {
  init() {
    // This used to crash when in an initializer, though not anywhere else.
    _ = StaticProperties.foo
    _ = StaticProperties.bar
    _ = StaticProperties.baz
  }
}

struct TestLetProperties {
  init(b: Burger) {
    _ = b.pattyCount
  }

  func flip(_ b: Burger) {
    _ = b.pattyCount
  }
}

// <rdar://problem/21933630> - setting let stored property in extension
// constructor
extension Burger {
  init(double pattyCount: Int) {
    self.pattyCount = pattyCount * 2
  }
}
