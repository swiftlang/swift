// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s -module-name foreach_async -swift-version 5  -disable-availability-checking  | %FileCheck %s
// REQUIRES: concurrency

//////////////////
// Declarations //
//////////////////

class C {}

@_silgen_name("loopBodyEnd")
func loopBodyEnd() -> ()

@_silgen_name("condition")
func condition() -> Bool

@_silgen_name("loopContinueEnd")
func loopContinueEnd() -> ()

@_silgen_name("loopBreakEnd")
func loopBreakEnd() -> ()

@_silgen_name("funcEnd")
func funcEnd() -> ()

struct TrivialStruct {
  var value: Int32
}

struct NonTrivialStruct {
  var value: C
}

struct GenericStruct<T> {
  var value: T
  var value2: C
}

protocol P {}
protocol ClassP : AnyObject {}

protocol GenericCollection : Collection {

}

struct AsyncLazySequence<S: Sequence>: AsyncSequence {
  typealias Element = S.Element
  typealias AsyncIterator = Iterator

  struct Iterator: AsyncIteratorProtocol {
    typealias Element = S.Element

    var iterator: S.Iterator?

    mutating func next() async -> S.Element? {
      return iterator?.next()
    }
  }

  var sequence: S

  func makeAsyncIterator() -> Iterator {
    return Iterator(iterator: sequence.makeIterator())
  }
}

///////////
// Tests //
///////////

//===----------------------------------------------------------------------===//
// Trivial Struct
//===----------------------------------------------------------------------===//

// CHECK-LABEL: sil hidden [ossa] @$s13foreach_async13trivialStructyyAA17AsyncLazySequenceVySaySiGGYaF : $@convention(thin) @async (@guaranteed AsyncLazySequence<Array<Int>>) -> () {
// CHECK: bb0([[SOURCE:%.*]] : @guaranteed $AsyncLazySequence<Array<Int>>):
// CHECK:   [[ITERATOR_BOX:%.*]] = alloc_box ${ var AsyncLazySequence<Array<Int>>.Iterator }, var, name "$x$generator"
// CHECK:   [[ITERATOR_LIFETIME:%.*]] = begin_borrow [var_decl] [[ITERATOR_BOX]]
// CHECK:   [[PROJECT_ITERATOR_BOX:%.*]] = project_box [[ITERATOR_LIFETIME]]
// CHECK:   br [[LOOP_DEST:bb[0-9]+]]

// CHECK: [[LOOP_DEST]]:
// CHECK:   [[NEXT_RESULT:%.*]] = alloc_stack $Optional<Int>
// CHECK:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK:   [[MUTATION:%.*]] = begin_access
// CHECK:   [[WITNESS_METHOD:%.*]] = function_ref @$sScIsE4next9isolation7ElementQzSgScA_pSgYi_tYa7FailureQzYKF : $@convention(method) @async <τ_0_0 where τ_0_0 : AsyncIteratorProtocol> (@sil_isolated @guaranteed Optional<any Actor>, @inout τ_0_0) -> (@out Optional<τ_0_0.Element>, @error_indirect τ_0_0.Failure)
// CHECK:   [[ERROR_SLOT:%[0-9]+]] = alloc_stack $Never
// CHECK:   try_apply [[WITNESS_METHOD]]<AsyncLazySequence<[Int]>.Iterator>([[NEXT_RESULT]], [[ERROR_SLOT]], [[ACTOR]], [[MUTATION]]) : $@convention(method) @async <τ_0_0 where τ_0_0 : AsyncIteratorProtocol> (@sil_isolated @guaranteed Optional<any Actor>, @inout τ_0_0) -> (@out Optional<τ_0_0.Element>, @error_indirect τ_0_0.Failure), normal [[NORMAL_BB:bb[0-2]+]], error [[ERROR_BB:bb[0-9]+]]
// CHECK: [[NORMAL_BB]](
// CHECK:   end_access [[MUTATION]]
// CHECK:   switch_enum [[IND_VAR:%.*]] : $Optional<Int>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]

// CHECK: [[SOME_BB]]([[VAR:%.*]] : $Int):
// CHECK:   loopBodyEnd
// CHECK:   br [[LOOP_DEST]]

// CHECK: [[NONE_BB]]:
// CHECK:   funcEnd
// CHECK:   return
// CHECK: } // end sil function '$s13foreach_async13trivialStructyyAA17AsyncLazySequenceVySaySiGGYaF'
func trivialStruct(_ xx: AsyncLazySequence<[Int]>) async {
  for await x in xx {
    loopBodyEnd()
  }
  funcEnd()
}

// CHECK-LABEL: sil hidden [ossa] @$s13foreach_async21trivialStructContinueyyAA17AsyncLazySequenceVySaySiGGYaF : $@convention(thin) @async (@guaranteed AsyncLazySequence<Array<Int>>) -> () {
// CHECK: bb0([[SOURCE:%.*]] : @guaranteed $AsyncLazySequence<Array<Int>>):
// CHECK:   [[ITERATOR_BOX:%.*]] = alloc_box ${ var AsyncLazySequence<Array<Int>>.Iterator }, var, name "$x$generator"
// CHECK:   [[PROJECT_ITERATOR_LIFETIME:%.*]] = begin_borrow [var_decl] [[ITERATOR_BOX]]
// CHECK:   [[PROJECT_ITERATOR_BOX:%.*]] = project_box [[ITERATOR_LIFETIME]]
// CHECK:   br [[LOOP_DEST:bb[0-9]+]]

// CHECK: [[LOOP_DEST]]:
// CHECK:   [[NEXT_RESULT:%.*]] = alloc_stack $Optional<Int>
// CHECK:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK:   [[MUTATION:%.*]] = begin_access
// CHECK:   [[WITNESS_METHOD:%.*]] = function_ref @$sScIsE4next9isolation7ElementQzSgScA_pSgYi_tYa7FailureQzYKF : $@convention(method) @async <τ_0_0 where τ_0_0 : AsyncIteratorProtocol> (@sil_isolated @guaranteed Optional<any Actor>, @inout τ_0_0) -> (@out Optional<τ_0_0.Element>, @error_indirect τ_0_0.Failure)
// CHECK:   [[ERROR_SLOT:%[0-9]+]] = alloc_stack $Never
// CHECK:   try_apply [[WITNESS_METHOD]]<AsyncLazySequence<[Int]>.Iterator>([[NEXT_RESULT]], [[ERROR_SLOT]], [[ACTOR]], [[MUTATION]]) : $@convention(method) @async <τ_0_0 where τ_0_0 : AsyncIteratorProtocol> (@sil_isolated @guaranteed Optional<any Actor>, @inout τ_0_0) -> (@out Optional<τ_0_0.Element>, @error_indirect τ_0_0.Failure), normal [[NORMAL_BB:bb[0-2]+]], error [[ERROR_BB:bb[0-9]+]]
// CHECK:  [[NORMAL_BB]](
// CHECK:   end_access [[MUTATION]]
// CHECK:   switch_enum [[IND_VAR:%.*]] : $Optional<Int>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]

// CHECK: [[SOME_BB]]([[VAR:%.*]] : $Int):
// CHECK:   condition
// CHECK:   cond_br [[VAR:%.*]], [[COND_TRUE:bb[0-9]+]], [[COND_FALSE:bb[0-9]+]]

// CHECK: [[COND_TRUE]]:
// CHECK:   loopContinueEnd
// CHECK:   br [[LOOP_DEST]]

// CHECK: [[COND_FALSE]]:
// CHECK:   loopBodyEnd
// CHECK:   br [[LOOP_DEST]]

// CHECK: [[NONE_BB]]:
// CHECK:   funcEnd
// CHECK:   return
// CHECK: } // end sil function '$s13foreach_async21trivialStructContinueyyAA17AsyncLazySequenceVySaySiGGYaF'

func trivialStructContinue(_ xx: AsyncLazySequence<[Int]>) async {
  for await x in xx {
    if (condition()) {
      loopContinueEnd()
      continue
    }
    loopBodyEnd()
  }

  funcEnd()
}

// TODO: Write this test
func trivialStructBreak(_ xx: AsyncLazySequence<[Int]>) async {
  for await x in xx {
    if (condition()) {
      loopBreakEnd()
      break
    }
    loopBodyEnd()
  }

  funcEnd()
}

// CHECK-LABEL: sil hidden [ossa] @$s13foreach_async26trivialStructContinueBreakyyAA17AsyncLazySequenceVySaySiGGYaF : $@convention(thin) @async (@guaranteed AsyncLazySequence<Array<Int>>) -> ()
// CHECK: bb0([[SOURCE:%.*]] : @guaranteed $AsyncLazySequence<Array<Int>>):
// CHECK:   [[ITERATOR_BOX:%.*]] = alloc_box ${ var AsyncLazySequence<Array<Int>>.Iterator }, var, name "$x$generator"
// CHECK:   [[ITERATOR_BOX_LIFETIME:%.*]] = begin_borrow [var_decl] [[ITERATOR_BOX]]
// CHECK:   [[PROJECT_ITERATOR_BOX:%.*]] = project_box [[ITERATOR_BOX_LIFETIME]]
// CHECK:   br [[LOOP_DEST:bb[0-9]+]]

// CHECK: [[LOOP_DEST]]:
// CHECK:   [[NEXT_RESULT:%.*]] = alloc_stack $Optional<Int>
// CHECK:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK:   [[MUTATION:%.*]] = begin_access
// CHECK:   [[WITNESS_METHOD:%.*]] = function_ref @$sScIsE4next9isolation7ElementQzSgScA_pSgYi_tYa7FailureQzYKF : $@convention(method) @async <τ_0_0 where τ_0_0 : AsyncIteratorProtocol> (@sil_isolated @guaranteed Optional<any Actor>, @inout τ_0_0) -> (@out Optional<τ_0_0.Element>, @error_indirect τ_0_0.Failure)
// CHECK:   [[ERROR_SLOT:%[0-9]+]] = alloc_stack $Never
// CHECK:   try_apply [[WITNESS_METHOD]]<AsyncLazySequence<[Int]>.Iterator>([[NEXT_RESULT]], [[ERROR_SLOT]], [[ACTOR]], [[MUTATION]]) : $@convention(method) @async <τ_0_0 where τ_0_0 : AsyncIteratorProtocol> (@sil_isolated @guaranteed Optional<any Actor>, @inout τ_0_0) -> (@out Optional<τ_0_0.Element>, @error_indirect τ_0_0.Failure), normal [[NORMAL_BB:bb[0-2]+]], error [[ERROR_BB:bb[0-9]+]]
// CHECK:  [[NORMAL_BB]](
// CHECK:   end_access [[MUTATION]]
// CHECK:   switch_enum [[IND_VAR:%.*]] : $Optional<Int>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]

// CHECK: [[SOME_BB]]([[VAR:%.*]] : $Int):
// CHECK:   condition
// CHECK:   cond_br [[VAR:%.*]], [[COND_TRUE:bb[0-9]+]], [[COND_FALSE:bb[0-9]+]]

// CHECK: [[COND_TRUE]]:
// CHECK:   loopBreakEnd
// CHECK:   br [[LOOP_EXIT:bb[0-9]+]]

// CHECK: [[COND_FALSE]]:
// CHECK:   condition
// CHECK:   cond_br [[VAR:%.*]], [[COND_TRUE2:bb[0-9]+]], [[COND_FALSE2:bb[0-9]+]]

// CHECK: [[COND_TRUE2]]:
// CHECK:   loopContinueEnd
// CHECK:   br [[LOOP_DEST]]

// CHECK: [[COND_FALSE2]]:
// CHECK:   br [[LOOP_DEST]]

// CHECK: [[LOOP_EXIT]]:
// CHECK:   return

// CHECK: } // end sil function '$s13foreach_async26trivialStructContinueBreakyyAA17AsyncLazySequenceVySaySiGGYaF'
func trivialStructContinueBreak(_ xx: AsyncLazySequence<[Int]>) async {
  for await x in xx {
    if (condition()) {
      loopBreakEnd()
      break
    }

    if (condition()) {
      loopContinueEnd()
      continue
    }
    loopBodyEnd()
  }

  funcEnd()
}
