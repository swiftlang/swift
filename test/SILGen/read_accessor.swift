// RUN: %target-swift-emit-silgen %s | %FileCheck %s

struct SimpleTest {
  var stored: String

  var readable: String {
// CHECK-LABEL: sil hidden @$s13read_accessor10SimpleTestV8readableSSvr
// CHECK-SAME:    : $@yield_once @convention(method) (@guaranteed SimpleTest) -> @yields @guaranteed String {
// CHECK:         [[T0:%.*]] = struct_extract %0 : $SimpleTest, #SimpleTest.stored
// CHECK-NEXT:    [[T1:%.*]] = copy_value [[T0]] : $String
// CHECK-NEXT:    yield [[T1]] : $String, resume bb1, unwind bb2
// CHECK:       bb1:
// CHECK-NEXT:    destroy_value [[T1]] : $String
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
// CHECK:       bb2:
// CHECK-NEXT:    destroy_value [[T1]] : $String
// CHECK-NEXT:    unwind
    _read {
      yield stored
    }
  }

// CHECK-LABEL: sil hidden @$s13read_accessor10SimpleTestV3getSSyF
// CHECK:         [[T0:%.*]] = begin_access [read] [unknown] %0
// CHECK-NEXT:    [[SELF:%.*]] = load [copy] [[T0]] : $*SimpleTest
// CHECK-NEXT:    end_access [[T0]]
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[READFN:%.*]] = function_ref @$s13read_accessor10SimpleTestV8readableSSvr : $@yield_once @convention(method) (@guaranteed SimpleTest) -> @yields @guaranteed String
// CHECK-NEXT:    ([[VALUE:%.*]], [[TOKEN:%.*]]) = begin_apply [[READFN]]([[SELF]])
//   FIXME: avoid this redundant materialization!
// CHECK-NEXT:    [[TEMP:%.*]] = alloc_stack $String
// CHECK-NEXT:    [[T0:%.*]] = copy_value [[VALUE]] : $String
// CHECK-NEXT:    store [[T0]] to [init] [[TEMP]] : $*String
// CHECK-NEXT:    [[RET:%.*]] = load [copy] [[TEMP]] : $*String
// CHECK-NEXT:    end_apply [[TOKEN]]
// CHECK-NEXT:    destroy_value [[SELF]]
// CHECK-NEXT:    destroy_addr [[TEMP]] : $*String
// CHECK-NEXT:    dealloc_stack [[TEMP]] : $*String
// CHECK-NEXT:    return [[RET]] : $String
  mutating func get() -> String {
    return readable
  }
}

class GetterSynthesis {
  var stored: String = "hello"
  var readable: String {
// CHECK: sil hidden [transparent] @$s13read_accessor15GetterSynthesisC8readableSSvg
// CHECK:         [[READFN:%.*]] = function_ref @$s13read_accessor15GetterSynthesisC8readableSSvr
// CHECK-NEXT:    ([[VALUE:%.*]], [[TOKEN:%.*]]) = begin_apply [[READFN]](%0)
//   FIXME: avoid this redundant materialization!
// CHECK-NEXT:    [[TEMP:%.*]] = alloc_stack $String
// CHECK-NEXT:    [[T0:%.*]] = copy_value [[VALUE]] : $String
// CHECK-NEXT:    store [[T0]] to [init] [[TEMP]] : $*String
// CHECK-NEXT:    [[RET:%.*]] = load [copy] [[TEMP]] : $*String
// CHECK-NEXT:    end_apply [[TOKEN]]
// CHECK-NEXT:    destroy_addr [[TEMP]] : $*String
// CHECK-NEXT:    dealloc_stack [[TEMP]] : $*String
// CHECK-NEXT:    return [[RET]] : $String
    _read {
      yield stored
    }
  }
}
