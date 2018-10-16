// RUN: %target-swift-frontend -emit-sil -enable-sil-ownership -disable-objc-attr-requires-foundation-module -enable-objc-interop %s | %FileCheck %s

// FIXME: This needs more tests

@objc protocol P3 {
  init?(p3: Int64)
}

extension P3 {
  // CHECK-LABEL: sil hidden @$s40definite_init_failable_initializers_objc2P3PAAE3p3axSgs5Int64V_tcfC : $@convention(method) <Self where Self : P3> (Int64, @thick Self.Type) -> @owned Optional<Self>
  init!(p3a: Int64) {
    self.init(p3: p3a)! // unnecessary-but-correct '!'
  }

  // CHECK-LABEL: sil hidden @$s40definite_init_failable_initializers_objc2P3PAAE3p3bxs5Int64V_tcfC : $@convention(method) <Self where Self : P3> (Int64, @thick Self.Type) -> @owned Self
  init(p3b: Int64) {
    self.init(p3: p3b)! // necessary '!'
  }
}

class LifetimeTracked {
  init(_: Int) {}
}

class FakeNSObject {
  @objc dynamic init() {}
}

class Cat : FakeNSObject {
  let x: LifetimeTracked

  // CHECK-LABEL: sil hidden @$s40definite_init_failable_initializers_objc3CatC1n5afterACSgSi_Sbtcfc : $@convention(method) (Int, Bool, @owned Cat) -> @owned Optional<Cat>
  // CHECK: bb0(%0 : $Int, %1 : $Bool, %2 : $Cat):
    // CHECK-NEXT: [[SELF_BOX:%.*]] = alloc_stack $Cat
    // CHECK:      store %2 to [[SELF_BOX]] : $*Cat
    // CHECK:      [[FIELD_ADDR:%.*]] = ref_element_addr %2 : $Cat, #Cat.x
    // CHECK-NEXT: store {{%.*}} to [[FIELD_ADDR]] : $*LifetimeTracked
    // CHECK-NEXT: [[COND:%.*]] = struct_extract %1 : $Bool, #Bool._value
    // CHECK-NEXT: cond_br [[COND]], bb1, bb2

  // CHECK: bb1:
    // CHECK-NEXT: br bb3

  // CHECK: bb2:
    // CHECK-NEXT: [[SUPER:%.*]] = upcast %2 : $Cat to $FakeNSObject
    // CHECK-NEXT: [[SUB:%.*]] = unchecked_ref_cast [[SUPER]] : $FakeNSObject to $Cat
    // CHECK-NEXT: [[SUPER_FN:%.*]] = objc_super_method [[SUB]] : $Cat, #FakeNSObject.init!initializer.1.foreign : (FakeNSObject.Type) -> () -> FakeNSObject, $@convention(objc_method) (@owned FakeNSObject) -> @owned FakeNSObject
    // CHECK-NEXT: [[NEW_SUPER_SELF:%.*]] = apply [[SUPER_FN]]([[SUPER]]) : $@convention(objc_method) (@owned FakeNSObject) -> @owned FakeNSObject
    // CHECK-NEXT: [[NEW_SELF:%.*]] = unchecked_ref_cast [[NEW_SUPER_SELF]] : $FakeNSObject to $Cat
    // CHECK-NEXT: store [[NEW_SELF]] to [[SELF_BOX]] : $*Cat
    // TODO: Once we re-enable arbitrary take promotion, this retain and the associated destroy_addr will go away.
    // CHECK-NEXT: strong_retain [[NEW_SELF]]
    // CHECK-NEXT: [[RESULT:%.*]] = enum $Optional<Cat>, #Optional.some!enumelt.1, [[NEW_SELF]] : $Cat
    // CHECK-NEXT: destroy_addr [[SELF_BOX]]
    // CHECK-NEXT: dealloc_stack [[SELF_BOX]] : $*Cat
    // CHECK-NEXT: br bb4([[RESULT]] : $Optional<Cat>)

  // CHECK: bb3:
    // CHECK-NEXT: [[FIELD_ADDR:%.*]] = ref_element_addr %2 : $Cat, #Cat.x
    // CHECK-NEXT: destroy_addr [[FIELD_ADDR]] : $*LifetimeTracked
    // CHECK-NEXT: [[METATYPE:%.*]] = metatype $@thick Cat.Type
    // CHECK-NEXT: dealloc_partial_ref %2 : $Cat, [[METATYPE]] : $@thick Cat.Type
    // CHECK-NEXT: dealloc_stack [[SELF_BOX]] : $*Cat
    // CHECK-NEXT: [[RESULT:%.*]] = enum $Optional<Cat>, #Optional.none!enumelt
    // CHECK-NEXT: br bb4([[RESULT]] : $Optional<Cat>)

  // CHECK: bb4([[RESULT:%.*]] : $Optional<Cat>):
    // CHECK-NEXT: return [[RESULT]] : $Optional<Cat>

  init?(n: Int, after: Bool) {
    self.x = LifetimeTracked(0)
    if after {
      return nil
    }
    super.init()
  }
}
