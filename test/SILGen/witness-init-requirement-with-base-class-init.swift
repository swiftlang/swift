// RUN: %target-swift-emit-silgen -enable-sil-ownership  %s | %FileCheck %s
// RUN: %target-swift-emit-sil -enable-sil-ownership -verify %s

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
// CHECK-LABEL: sil private [transparent] [thunk] @$S4main3DogCAA10BestFriendA2aDPxycfCTW
// CHECK:         [[SELF:%.*]] = apply
// CHECK:         unchecked_ref_cast [[SELF]] : $Animal to $Dog
// CHECK-LABEL: sil private [transparent] [thunk] @$S4main3DogCAA10BestFriendA2aDP6createxyFZTW
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

// CHECK-LABEL: sil hidden @$S4main4BaseC1xACSi_tcfC : $@convention(method) (Int, @thick Base.Type) -> @owned Base
// CHECK:         [[SELF:%.*]] = alloc_ref_dynamic %1 : $@thick Base.Type, $Base
// CHECK:         [[METHOD:%.*]] = function_ref @$S4main4BaseC1xACSi_tcfc
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[METHOD]](%0, [[SELF]])
// CHECK-NEXT:    return [[RESULT]]

// CHECK-LABEL: sil private [transparent] [thunk] @$S4main7DerivedCAA8InitableA2aDP1xxSi_tcfCTW : $@convention(witness_method: Initable) (Int, @thick Derived.Type) -> @out Derived
// CHECK:         [[SELF:%.*]] = upcast %2 : $@thick Derived.Type to $@thick Base.Type
// CHECK:         [[METHOD:%.*]] = function_ref @$S4main4BaseC1xACSi_tcfC
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[METHOD]](%1, [[SELF]])
// CHECK-NEXT:    [[NEW_SELF:%.*]] = unchecked_ref_cast [[RESULT]] : $Base to $Derived
// CHECK-NEXT:    store [[NEW_SELF]] to [init] %0 : $*Derived
// CHECK-NEXT:    [[TUPLE:%.*]] = tuple ()
// CHECK-NEXT:    return [[TUPLE]]
