// RUN: %target-swift-emit-silgen %s | %FileCheck %s

struct SimpleTest {
  var stored: String

  var readable: String {
// CHECK-LABEL: sil hidden @$s13read_accessor10SimpleTestV8readableSSvr
// CHECK-SAME:    : $@yield_once @convention(method) (@guaranteed SimpleTest) -> @yields @guaranteed String {
// CHECK:         [[T0:%.*]] = struct_extract %0 : $SimpleTest, #SimpleTest.stored
// CHECK-NEXT:    yield [[T0]] : $String, resume bb1, unwind bb2
// CHECK:       bb1:
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
// CHECK:       bb2:
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
// CHECK-NEXT:    [[RET:%.*]] = copy_value [[VALUE]] : $String
// CHECK-NEXT:    end_apply [[TOKEN]]
// CHECK-NEXT:    destroy_value [[SELF]]
// CHECK-NEXT:    return [[RET]] : $String
  mutating func get() -> String {
    return readable
  }
}

class GetterSynthesis {
  var stored: String = "hello"
  var readable: String {
// CHECK-LABEL: sil hidden [transparent] @$s13read_accessor15GetterSynthesisC8readableSSvg
// CHECK:         [[READFN:%.*]] = function_ref @$s13read_accessor15GetterSynthesisC8readableSSvr
// CHECK-NEXT:    ([[VALUE:%.*]], [[TOKEN:%.*]]) = begin_apply [[READFN]](%0)
// CHECK-NEXT:    [[RET:%.*]] = copy_value [[VALUE]] : $String
// CHECK-NEXT:    end_apply [[TOKEN]]
// CHECK-NEXT:    return [[RET]] : $String
    _read {
      yield stored
    }
  }
}

func void() {}

struct TupleReader {
  var stored: String

  subscript(i: Int) -> String {
    _read { yield stored }
  }

  func compute() -> String { return stored }
  func index() -> Int { return 0 }

  var readable: ((String, ()), String, ()) {
// CHECK-LABEL: sil hidden @$s13read_accessor11TupleReaderV8readableSS_ytt_SSyttvr
// CHECK:         debug_value %0
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[INDEXFN:%.*]] = function_ref @$s13read_accessor11TupleReaderV5indexSiyF
// CHECK-NEXT:    [[INDEX:%.*]] = apply [[INDEXFN]](%0)
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[COMPUTEFN:%.*]] = function_ref @$s13read_accessor11TupleReaderV7computeSSyF
// CHECK-NEXT:    [[COMPUTE:%.*]] = apply [[COMPUTEFN]](%0)
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[VOIDFN:%.*]] = function_ref @$s13read_accessor4voidyyF
// CHECK-NEXT:    apply [[VOIDFN]]()
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[SUBREADFN:%.*]] = function_ref @$s13read_accessor11TupleReaderVySSSicir
// CHECK-NEXT:    ([[SUBREAD:%.*]], [[SUBTOKEN:%.*]]) = begin_apply [[SUBREADFN]]([[INDEX]], %0)
// CHECK-NEXT:    yield ([[SUBREAD]] : $String, [[COMPUTE]] : $String), resume bb1, unwind bb2
// CHECK:       bb1:
// CHECK-NEXT:    end_apply [[SUBTOKEN]]
// CHECK-NEXT:    destroy_value [[COMPUTE]] : $String
// CHECK-NEXT:    [[T0:%.*]] = tuple ()
// CHECK-NEXT:    return [[T0]] : $()
// CHECK:       bb2:
//   Should this be an abort_apply?
// CHECK-NEXT:    end_apply [[SUBTOKEN]]
// CHECK-NEXT:    destroy_value [[COMPUTE]] : $String
// CHECK-NEXT:    unwind
// CHECK-LABEL: } // end sil function '$s13read_accessor11TupleReaderV8readableSS_ytt_SSyttvr'
    _read {
      yield (((self[index()], ()), compute(), void()))
    }
  }

// CHECK-LABEL: sil hidden @$s13read_accessor11TupleReaderV11useReadableyyF
// CHECK:         [[READFN:%.*]] = function_ref @$s13read_accessor11TupleReaderV8readableSS_ytt_SSyttvr
// CHECK-NEXT:    ([[FIRST:%.*]], [[SECOND:%.*]], [[TOKEN:%.*]]) = begin_apply [[READFN]](%0)
//   FIXME: this materialization is silly
// CHECK-NEXT:    [[TEMP:%.*]] = alloc_stack $((String, ()), String, ())
// CHECK-NEXT:    [[TEMP_0:%.*]] = tuple_element_addr [[TEMP]] : $*((String, ()), String, ()), 0
// CHECK-NEXT:    [[TEMP_1:%.*]] = tuple_element_addr [[TEMP]] : $*((String, ()), String, ()), 1
// CHECK-NEXT:    [[TEMP_2:%.*]] = tuple_element_addr [[TEMP]] : $*((String, ()), String, ()), 2
// CHECK-NEXT:    [[TEMP_0_0:%.*]] = tuple_element_addr [[TEMP_0]] : $*(String, ()), 0
// CHECK-NEXT:    [[TEMP_0_1:%.*]] = tuple_element_addr [[TEMP_0]] : $*(String, ()), 1
// CHECK-NEXT:    [[T0:%.*]] = copy_value [[FIRST]] : $String
// CHECK-NEXT:    store [[T0]] to [init] [[TEMP_0_0]]
// CHECK-NEXT:    [[T0:%.*]] = copy_value [[SECOND]] : $String
// CHECK-NEXT:    store [[T0]] to [init] [[TEMP_1]]
// CHECK-NEXT:    [[TUPLE:%.*]] = load [copy] [[TEMP]]
// CHECK-NEXT:    destructure_tuple
// CHECK-NEXT:    destructure_tuple
// CHECK-NEXT:    end_apply
// CHECK-LABEL: } // end sil function '$s13read_accessor11TupleReaderV11useReadableyyF'
  func useReadable() {
    var v = readable
  }
}
