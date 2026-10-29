// RUN: %target-swift-emit-silgen -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -module-name main %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -module-name main -D WITH_ATTR %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// '@oneway(never)' does not affect the function type, so adding or removing
// it does not change the mangled names of a function, a method, a protocol
// requirement or a function value of its type

#if WITH_ATTR
@oneway(never) func flush() {}

actor Worker {
  @oneway(never) func drain() {}
}

protocol Drainable {
  @oneway(never) func drain()
}
#else
func flush() {}

actor Worker {
  func drain() {}
}

protocol Drainable {
  func drain()
}
#endif

// CHECK-LABEL: sil hidden [ossa] @$s4main5flushyyF : $@convention(thin) () -> () {
// CHECK-LABEL: sil hidden [ossa] @$s4main6WorkerC5drainyyF : $@convention(method) (@sil_isolated @guaranteed Worker) -> () {

// CHECK-LABEL: sil hidden [ossa] @$s4main9takeValueyyyycF : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> () {
func takeValue(_ fn: @escaping () -> Void) {}

// CHECK-LABEL: sil hidden [ossa] @$s4main4useDyyxAA9DrainableRzlF :
func useD<D: Drainable>(_ d: D) {
  // CHECK: witness_method $D, #Drainable.drain : <Self where Self : Drainable> (Self) -> () -> ()
  d.drain()
  takeValue(flush)
}
