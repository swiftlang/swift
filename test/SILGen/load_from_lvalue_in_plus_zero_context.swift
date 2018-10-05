// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

class A {
  lazy var b: B = B()
}

final class B {
  var c: C? {
    get { return nil }
    set {}
  }
}

struct C {
  let d: String
}

// CHECK-LABEL: sil hidden @$s37load_from_lvalue_in_plus_zero_context4test1ayAA1AC_tF : $@convention(thin) (@guaranteed A) -> () {
func test(a: A) {
  let s: String?
  // CHECK:   [[C_TEMP:%.*]] = alloc_stack $Optional<C>
  // CHECK:   [[HASVALUE:%.*]] = select_enum_addr [[C_TEMP]]
  // CHECK:   cond_br [[HASVALUE]], [[SOME:bb[0-9]+]],
  //
  // CHECK: [[SOME]]:
  // CHECK:   [[C_PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[C_TEMP]]
  // -- This must be a copy, since we'll immediately destroy the value in the
  //    temp buffer
  // CHECK:   [[LOAD:%.*]] = load [copy] [[C_PAYLOAD]]
  // CHECK:   destroy_addr [[C_TEMP]]
  s = a.b.c?.d
  print(s)
}
