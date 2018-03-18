// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden @{{.*}}test
func test(a: A) {
  let s: String?
  // CHECK:   [[C_TEMP:%.*]] = alloc_stack $Optional<C>
  // CHECK:   [[C_TEMP_TEMP:%.*]] = alloc_stack $Optional<C>
  // CHECK:   copy_addr [[C_TEMP]] to [initialization] [[C_TEMP_TEMP]]
  // CHECK:   [[C_TEMP_TEMP_LOADED:%.*]] = load [take] [[C_TEMP_TEMP]]
  // CHECK:   switch_enum [[C_TEMP_TEMP_LOADED]] : $Optional<C>, case #Optional.some!enumelt.1: [[SOME:bb[0-9]+]], case #Optional.none!enumelt: [[NONE:bb[0-9]+]]
  //
  // CHECK: [[SOME]](
  // CHECK:   [[C_PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[C_TEMP]]
  // -- This must be a copy, since we'll immediately destroy the value in the
  //    temp buffer
  // CHECK:   [[LOAD:%.*]] = load [copy] [[C_PAYLOAD]]
  // CHECK:   destroy_addr [[C_TEMP]]
  s = a.b.c?.d
  print(s)
}
