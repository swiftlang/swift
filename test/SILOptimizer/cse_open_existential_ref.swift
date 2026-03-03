// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s

protocol P {}

extension P {
  func extensionMethod() -> Self {
    return self
  }
}

class C : P {}

@_optimize(none)
func blackHole<T>(_: T) {}

public func caller() {
  let value: any P & AnyObject = C()
  blackHole(value === value.extensionMethod())
}

// We generate sub-optimal SIL here, but make sure we don't crash
// at least:

// CHECK-LABEL: sil @$s24cse_open_existential_ref6calleryyF : $@convention(thin) () -> () {
// CHECK: open_existential_ref
// CHECK: init_existential_ref
// CHECK: function_ref @$s24cse_open_existential_ref1PPAAE15extensionMethodxyF
// CHECK: init_existential_ref
// CHECK: open_existential_ref
// CHECK: init_existential_ref
// CHECK: ref_to_raw_pointer
// CHECK: ref_to_raw_pointer
// CHECK: builtin "cmp_eq_RawPointer"
// CHECK: function_ref @$s24cse_open_existential_ref9blackHoleyyxlF
