// RUN: %target-swift-emit-silgen %s | %FileCheck %s
// RUN: %target-swift-emit-sil %s -sil-verify-all

// Test that consuming switch on address only enum with trivial payload case correctly use load [trivial]
// instead of load [take].

protocol AProtocol {}

enum AddressOnly: ~Copyable {
  case unloadableAndInt(any AProtocol, Int)
}

// CHECK-LABEL: sil hidden [ossa] @$s33address_only_enum_trivial_payload14consumeTheEnum
func consumeTheEnum(_ value: consuming AddressOnly) -> Int {
  // CHECK: switch_enum_addr {{.*}}AddressOnly.unloadableAndInt
  switch consume value {
  case .unloadableAndInt(_, let count):
    // CHECK: [[ADDR:%[0-9]+]] = unchecked_take_enum_data_addr
    // CHECK: [[TRIVIAL_ADDR:%[0-9]+]] = tuple_element_addr [[ADDR]], 1
    // CHECK: load [trivial] [[TRIVIAL_ADDR]]
    return count
  }
}
