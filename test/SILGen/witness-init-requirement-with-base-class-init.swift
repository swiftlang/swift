// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types  %s | %FileCheck %s
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -verify %s

protocol BestFriend: class {
  init()
  static func create() -> Self
}

class Animal {
  required init(species: String) {}

  static func create() -> Self { return self.init() }
  required convenience init() { self.init(species: "\(type(of: self))") }
}

class Dog: Animal, BestFriend {}
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4main3DogCAA10BestFriendA2aDPxycfCTW
// CHECK:         [[SELF:%.*]] = apply
// CHECK:         unchecked_ref_cast [[SELF]] : $Animal to $Dog
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4main3DogCAA10BestFriendA2aDP6createxyFZTW
// CHECK:         [[SELF:%.*]] = apply
// CHECK:         unchecked_ref_cast [[SELF]] : $Animal to $Dog

class Base {
  init() {}

  convenience init(x: Int) {
    self.init()
  }
}

protocol Initable {
  init(x: Int)
}

final class Derived : Base, Initable {}

// CHECK-LABEL: sil hidden [ossa] @$s4main4BaseC1xACSi_tcfC : $@convention(method) (Int, @thick Base.Type) -> @owned Base
// CHECK:         [[METHOD:%.*]] = class_method [[SELF_META:%.*]] : $@thick Base.Type, #Base.init!allocator
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[METHOD]]([[SELF_META]])
// CHECK-NEXT:    assign [[RESULT]] to [[BOX:%.*]] :
// CHECK-NEXT:    [[FINAL:%.*]] = load [copy] [[BOX]]
// CHECK:         return [[FINAL]]

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4main7DerivedCAA8InitableA2aDP1xxSi_tcfCTW :
// CHECK:         [[SELF:%.*]] = upcast %2 : $@thick Derived.Type to $@thick Base.Type
// CHECK:         [[METHOD:%.*]] = function_ref @$s4main4BaseC1xACSi_tcfC
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[METHOD]](%1, [[SELF]])
// CHECK-NEXT:    [[NEW_SELF:%.*]] = unchecked_ref_cast [[RESULT]] : $Base to $Derived
// CHECK-NEXT:    store [[NEW_SELF]] to [init] %0 : $*Derived
// CHECK-NEXT:    [[TUPLE:%.*]] = tuple ()
// CHECK-NEXT:    return [[TUPLE]]
