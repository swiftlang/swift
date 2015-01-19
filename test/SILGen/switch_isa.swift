// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// rdar://17772217
func testSwitchOnExistential(value: Any) {
  switch(value) {
    case true as Bool: println("true")
    case false as Bool: println("false")
    default: println("default")
  }
}

// CHECK: sil hidden @_TF10switch_isa23testSwitchOnExistentialFP_T_ :
// CHECK:   [[ANY:%.*]] = alloc_stack $protocol<>
// CHECK:   copy_addr %0 to [initialization] [[ANY]]#1
// CHECK:   [[BOOL:%.*]] = alloc_stack $Bool
// CHECK:   checked_cast_addr_br copy_on_success protocol<> in [[ANY]]#1 : $*protocol<> to Bool in [[BOOL]]#1 : $*Bool, [[IS_BOOL:bb[0-9]+]], [[IS_NOT_BOOL:bb[0-9]+]]
// CHECK: [[IS_BOOL]]:
// CHECK:   [[T0:%.*]] = load [[BOOL]]#1
