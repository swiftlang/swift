// RUN: %target-swift-emit-silgen -module-name test \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature AddressableTypes %s | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableTypes

struct View: ~Escapable {
  var p: Int

  @_lifetime(immortal)
  init() { p = 0 }

  var isEmpty: Bool { p == 0 }
}

@_addressableForDependencies
struct Base {
  var id: String

  var view: View {
    @_lifetime(borrow self)
    borrowing get { _overrideLifetime(View(), borrowing: self) }
  }
}

// A base with no addressable storage.
struct NoAddr {
  var n: Int

  var view: View {
    @_lifetime(borrow self)
    borrowing get { _overrideLifetime(View(), borrowing: self) }
  }
}

func f(_ s: View?) {}

// Non-Escapable value consumed inside a chain whose result is escapable.
//
// CHECK-LABEL: sil hidden [ossa] @$s4test8consumed{{.*}} : $@convention(thin) (@guaranteed Optional<Base>) -> Optional<Bool> {
// CHECK:         [[COPY:%.*]] = copy_value %0
// CHECK:         switch_enum [[COPY]], case #Optional.some!enumelt: bb1, case #Optional.none!enumelt: bb3
// CHECK:       bb1({{.*}} : @owned $Base):
// CHECK-LABEL: } // end sil function '$s4test8consumed{{.*}}'
func consumed(_ b: Base?) -> Bool? {
  b?.view.isEmpty
}

// The optional is passed by value because the base isn't addressable.
//
// CHECK-LABEL: sil hidden [ossa] @$s4test6noAddr{{.*}} : $@convention(thin) (Optional<NoAddr>) -> () {
// CHECK:         switch_enum %0, case #Optional.some!enumelt: bb1, case #Optional.none!enumelt: bb3
// CHECK:       bb1({{.*}} : $NoAddr):
// CHECK-NOT:     store_borrow
// CHECK:         apply {{%.*}}({{%.*}}) : $@convention(method) (NoAddr) -> @lifetime(borrow 0) @owned View
// CHECK-LABEL: } // end sil function '$s4test6noAddr{{.*}}'
func noAddr(_ b: NoAddr?) {
  f(b?.view)
}
