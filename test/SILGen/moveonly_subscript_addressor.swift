// RUN: %target-swift-emit-silgen %s | %FileCheck %s

struct Foo: ~Copyable {
  let value: Int

  @_silgen_name("load")
  borrowing func load() -> Int
}

// CHECK-LABEL: sil {{.*}} @${{.*}}4load
func load(b: UnsafeMutableBufferPointer<Foo>) -> Int {
    // Ensure the borrowing invocation of `load` happens within the access to
    // the pointed-at memory.
    // CHECK: [[PTR:%.*]] = pointer_to_address
    // CHECK: [[MD:%.*]] = mark_dependence [unresolved] [[PTR]] on %0
    // CHECK: [[BEGIN:%.*]] = begin_access [read] [unsafe] [[MD]]
    // CHECK: [[FN:%.*]] = function_ref @load
    // CHECK: apply [[FN]]
    // CHECK: end_access [[BEGIN]]
    return b[1].load()
}
