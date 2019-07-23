// RUN: %target-swift-frontend -disable-availability-checking -emit-silgen %s | %FileCheck %s

protocol P {}
protocol Q: AnyObject {}

extension String: P {}
struct AddrOnly: P { var field: P }

class C: Q {}

// CHECK-LABEL: sil hidden {{.*}}11valueToAddr1xQr
func valueToAddr(x: String) -> some P {
  // CHECK: [[UNDERLYING:%.*]] = unchecked_addr_cast %0
  // CHECK: [[VALUE_COPY:%.*]] = copy_value %1
  // CHECK: store [[VALUE_COPY]] to [init] [[UNDERLYING]]
  return x
}

// CHECK-LABEL: sil hidden {{.*}}10addrToAddr1xQr
func addrToAddr(x: AddrOnly) -> some P {
  // CHECK: [[UNDERLYING:%.*]] = unchecked_addr_cast %0
  // CHECK: copy_addr %1 to [initialization] [[UNDERLYING]]
  return x
}

// CHECK-LABEL: sil hidden {{.*}}13genericAddrToE01xQr
func genericAddrToAddr<T: P>(x: T) -> some P {
  // CHECK: [[UNDERLYING:%.*]] = unchecked_addr_cast %0
  // CHECK: copy_addr %1 to [initialization] [[UNDERLYING]]
  return x
}

// CHECK-LABEL: sil hidden {{.*}}12valueToValue1xQr
func valueToValue(x: C) -> some Q {
  // CHECK: [[VALUE_COPY:%.*]] = copy_value %0
  // CHECK: [[CAST_TO_OPAQUE:%.*]] = unchecked_ref_cast [[VALUE_COPY]]
  // CHECK: return [[CAST_TO_OPAQUE]]
  return x
}

// CHECK-LABEL: sil hidden {{.*}}13reabstraction1xQr
func reabstraction(x: @escaping () -> ()) -> some Any {
  // CHECK: [[UNDERLYING:%.*]] = unchecked_addr_cast %0 : ${{.*}} to $*@callee_guaranteed () -> @out ()
  // CHECK: [[VALUE_COPY:%.*]] = copy_value %1
  // CHECK: [[VALUE_REABSTRACT:%.*]] = partial_apply [callee_guaranteed] {{%.*}}([[VALUE_COPY]])
  // CHECK: store [[VALUE_REABSTRACT]] to [init] [[UNDERLYING]]
  return x
}
