// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-object -emit-module -o %t %S/Inputs/def_class.swift -disable-objc-attr-requires-foundation-module
// RUN: llvm-bcanalyzer %t/def_class.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -Xllvm -sil-disable-pass="External Defs To Decls" -sil-debug-serialization -I %t %s | %FileCheck %s -check-prefix=SIL
// RUN: echo "import def_class; struct A : ClassProto {}" | not %target-swift-frontend -typecheck -I %t - 2>&1 | %FileCheck %s -check-prefix=CHECK-STRUCT

// CHECK-NOT: UnknownCode
// CHECK-STRUCT: non-class type 'A' cannot conform to class protocol 'ClassProto'

// Make sure we can "merge" def_class.
// RUN: %target-swift-frontend -emit-module -o %t-merged.swiftmodule %t/def_class.swiftmodule -module-name def_class

import def_class

var a : Empty
var b = TwoInts(a: 1, b: 2)
var computedProperty : ComputedProperty
var sum = b.x + b.y + computedProperty.value

var intWrapper = ResettableIntWrapper()
var r : Resettable = intWrapper
r.reset()
r.doReset()

class AnotherIntWrapper : SpecialResettable, ClassProto {
  init() { value = 0 }
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

var c : Cacheable = intWrapper2
c.compute()
c.reset()


var p = Pair(a: 1, b: 2.5)
p.first = 2
p.second = 5.0

struct Int {}

var gc = GenericCtor<Int>()
gc.doSomething()


a = StillEmpty()
r = StillEmpty()

var bp = BoolPair<Bool>()
bp.bothTrue()

var rawBP : Pair<Bool, Bool>
rawBP = bp


var rev : SpecialPair<Double>
rev.first = 42
var comp : Computable = rev

var simpleSub = ReadonlySimpleSubscript()
var subVal = simpleSub[4]

var complexSub = ComplexSubscript()
complexSub[4, false] = complexSub[3, true]

var rsrc = Resource()

getReqPairLike()

// SIL-LABEL: sil public_external [transparent] [fragile] @_T0s1poiS2i_SitF : $@convention(thin) (Int, Int) -> Int {

func test(_ sharer: ResourceSharer) {}

class HasNoOptionalReqs : ObjCProtoWithOptional { }

HasNoOptionalReqs()
OptionalImplementer().unrelated()

extension def_class.ComputedProperty { }
