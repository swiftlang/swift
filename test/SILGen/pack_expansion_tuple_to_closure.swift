// RUN: %target-swift-emit-silgen %s -target %target-swift-5.9-abi-triple | %FileCheck %s

// Test that passing a pack expansion tuple (from iteration or direct passing)
// to a closure correctly uses pack_element_set to initialize the pack.
//
// This is a regression test for a bug where SILGen used pack_element_get
// to read from an uninitialized pack, then tried to copy to that garbage
// address, corrupting the stack frame.

struct FuzzEngine<each Input> {
  // CHECK-LABEL: sil hidden [ossa] @$s31pack_expansion_tuple_to_closure10FuzzEngineV3run6inputs4testySayxxQp_tG_yxxQp_t_tKXEtKF
  // The loop body should use pack_element_set to initialize the pack, not pack_element_get.
  // CHECK: [[PACK:%[0-9]+]] = alloc_pack $Pack{repeat each Input}
  // CHECK: [[TUPLE_ELT:%[0-9]+]] = tuple_pack_element_addr
  // CHECK: pack_element_set [[TUPLE_ELT]] into {{%[0-9]+}} of [[PACK]]
  // CHECK: try_apply {{%[0-9]+}}([[PACK]])
  func run(inputs: [(repeat each Input)], test: ((repeat each Input)) throws -> Void) rethrows {
    for input in inputs {
      try test(input)
    }
  }
}

struct Runner<each Input> {
  // CHECK-LABEL: sil hidden [ossa] @$s31pack_expansion_tuple_to_closure6RunnerV3run_4testyxxQp_t_yxxQp_t_tKXEtKF
  // Direct passing should also use pack_element_set.
  // CHECK: [[PACK2:%[0-9]+]] = alloc_pack $Pack{repeat each Input}
  // CHECK: pack_element_set {{%[0-9]+}} into {{%[0-9]+}} of [[PACK2]]
  // CHECK: try_apply {{%[0-9]+}}([[PACK2]])
  func run(_ input: (repeat each Input), test: ((repeat each Input)) throws -> Void) rethrows {
    try test(input)
  }
}

func useIt() {
  let engine = FuzzEngine<Int>()
  engine.run(inputs: [(42)]) { value in
    print(value)
  }

  let runner = Runner<Int>()
  runner.run((99)) { value in
    print(value)
  }
}
