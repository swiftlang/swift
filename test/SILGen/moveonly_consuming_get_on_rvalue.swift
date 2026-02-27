// RUN: %target-swift-emit-silgen -module-name test -enable-experimental-feature Lifetimes %s | %FileCheck %s
// RUN: %target-swift-emit-sil -module-name test -enable-experimental-feature Lifetimes %s -sil-verify-all

// REQUIRES: swift_feature_Lifetimes

// Test that calling a consuming accessor on a noncopyable rvalue passes the owned value directly
// instead of initiating a borrow scope and copying the value within it.

// The tests use forcing the value out of an optional as the means to get the rvalue.

struct NC: ~Copyable {
  var nc2: NC2 = NC2()
  
  @_owned var value: NC2 {
    consuming get { NC2() }
  }
}

struct NC2: ~Copyable {
  init() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s4test14consume_objectyyF
// CHECK:       switch_enum {{%.*}}, case #Optional.some!enumelt: [[BB:bb[0-9]+]]
// CHECK:       [[BB]]([[OWNED_NC:%.*]] : @owned $NC):
// CHECK-NOT:     begin_borrow
// CHECK-NOT:     copy_value
// CHECK:         = apply {{%.*}}([[OWNED_NC]])
// CHECK:       } // end sil function
func consume_object() {
  let nc: NC? = NC()
  let _ = nc!.value
}

struct AddressOnlyNC: ~Copyable {
  // Here to make the value address only.
  var _x: Any?
  
  @_owned var value: NC? {
    consuming get {
      NC()
    }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test15consume_addressyyF
// CHECK:       switch_enum_addr [[ENUM_ADDR:%.*]], case #Optional.some!enumelt: [[BB:bb[0-9]+]]
// CHECK:       [[BB]]:
// CHECK-NOT:     copy_addr {{%.*}} to
// CHECK:         [[TAKEN:%.*]] = unchecked_take_enum_data_addr [[ENUM_ADDR]]
// CHECK:         = apply {{%.*}}([[TAKEN]])
// CHECK:       } // end sil function
func consume_address() {
  let nc: AddressOnlyNC? = AddressOnlyNC()
  let x = nc!.value
  _ = x
}

// Original reproducing case:

struct X: ~Escapable, ~Copyable {
  @_lifetime(immortal)
  init() {}
}

struct MutableView: ~Copyable, ~Escapable {
  @_lifetime(immortal)
  init() {}
  
  @_owned var x: X {
    @_lifetime(copy self)
    consuming get {
      X()
    }
  }
}

struct Source: ~Copyable {
  @_lifetime(&self)
  mutating func input() -> MutableView? {
    MutableView()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test21mutateValueFromSourceyyF
// CHECK:       switch_enum {{%.*}}, case #Optional.some!enumelt: [[BB:bb[0-9]+]]
// CHECK:       [[BB]]([[OWNED_MV:%.*]] : @owned $MutableView):
// CHECK-NOT:     begin_borrow
// CHECK-NOT:     copy_value
// CHECK:         = apply {{%.*}}([[OWNED_MV]])
// CHECK:       } // end sil function
func mutateValueFromSource() {
  var source = Source()
  let mv = source.input()
  
  let x = mv!.x
  _ = x
}
