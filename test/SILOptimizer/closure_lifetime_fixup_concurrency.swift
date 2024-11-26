// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -sil-verify-all  -target %target-swift-5.1-abi-triple -Xllvm -sil-print-types -emit-sil -enable-copy-propagation=false -I %t -o - | %FileCheck %s
// REQUIRES: concurrency

// CHECK-LABEL: sil @$s34closure_lifetime_fixup_concurrency12testAsyncLetyS2SYaF : $@convention(thin) @async (@guaranteed String) -> @owned String {
// CHECK:   [[PA:%.*]] = partial_apply [callee_guaranteed] [on_stack]
// CHECK:   [[MD:%.*]] = mark_dependence [[PA]]
// CHECK:   [[CONV:%.*]] = convert_function [[MD]]
// CHECK:   [[BAL:%.*]] = builtin "startAsyncLetWithLocalBuffer"<String>([[OPT:%.+]] : $Optional<Builtin.RawPointer>, [[CONV]]
// CHECK:   builtin "endAsyncLetLifetime"([[BAL]] : $Builtin.RawPointer, [[CONV]]
// CHECK: } // end sil function '$s34closure_lifetime_fixup_concurrency12testAsyncLetyS2SYaF'

public func testAsyncLet(_ n: String) async -> String {
  async let first = n
  let result = await first
  return result
}
