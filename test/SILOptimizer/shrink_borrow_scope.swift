// RUN: %target-swift-frontend -c -O -enable-copy-propagation=true -enable-lexical-lifetimes=true -sil-verify-all -Xllvm -sil-print-final-ossa-module %s | %FileCheck %s

// =============================================================================
// = DECLARATIONS                                                             {{
// =============================================================================

@_silgen_name("consumeAndProduce")
func consumeAndProduce(_ arg: __owned AnyObject) -> AnyObject

// =============================================================================
// = DECLARATIONS                                                             }}
// =============================================================================

// =============================================================================
// = TESTS                                                                    {{
// =============================================================================

@_silgen_name("eliminate_copy_of_returned_then_consumed_owned_value")
public func eliminate_copy_of_returned_then_consumed_owned_value(arg: __owned AnyObject) {
  // CHECK-LABEL: sil [ossa] @eliminate_copy_of_returned_then_consumed_owned_value : {{.*}} {
  // CHECK:       {{bb[0-9]+}}([[ARG:%[^,]+]] : @owned $AnyObject):
  // retain arg
  // CHECK:       [[ARG_LIFETIME:%[^,]+]] = begin_borrow [lexical] [[ARG]]
  // CHECK:       [[ARG_COPY:%[^,]+]] = copy_value [[ARG_LIFETIME]]
  let x = consumeAndProduce(arg)
  // CHECK:       [[X:%[^,]+]] = apply {{%[^,]+}}([[ARG_COPY]])
  // CHECK:       [[MOVE_X:%[^,]+]] = move_value [lexical] [[X]]
  // no copy of 'x'
  _ = consumeAndProduce(x)
  // CHECK:       [[RESULT:%[^,]+]] = apply {{%[^,]+}}([[MOVE_X]])
  // CHECK:       end_borrow [[ARG_LIFETIME]]
  // release result
  // release arg
  // CHECK:       destroy_value [[ARG]]
  // CHECK:       destroy_value [[RESULT]]
  // CHECK-LABEL: } // end sil function 'eliminate_copy_of_returned_then_consumed_owned_value'
}

// =============================================================================
// = TESTS                                                                    }}
// =============================================================================
