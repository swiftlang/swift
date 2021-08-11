// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -I %S/Inputs/custom-modules  -disable-availability-checking %s -verify | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-%target-cpu %s
// REQUIRES: concurrency
// REQUIRES: objc_interop

import Foundation
import ObjCConcurrency

// CHECK-LABEL: sil {{.*}}@${{.*}}14testSlowServer
func testSlowServer(slowServer: SlowServer) async throws {
  // CHECK: [[RESUME_BUF:%.*]] = alloc_stack $Int
  // CHECK: [[STRINGINIT:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF :
  // CHECK: [[ARG:%.*]] = apply [[STRINGINIT]]
  // CHECK: [[METHOD:%.*]] = objc_method {{.*}} $@convention(objc_method) (NSString, @convention(block) (Int) -> (), SlowServer) -> ()
  // CHECK: [[CONT:%.*]] = get_async_continuation_addr Int, [[RESUME_BUF]]
  // CHECK: [[WRAPPED:%.*]] = struct $UnsafeContinuation<Int, Never> ([[CONT]] : $Builtin.RawUnsafeContinuation)
  // CHECK: [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage UnsafeContinuation<Int, Never>
  // CHECK: [[CONT_SLOT:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK: store [[WRAPPED]] to [trivial] [[CONT_SLOT]]
  // CHECK: [[BLOCK_IMPL:%.*]] = function_ref @[[INT_COMPLETION_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage UnsafeContinuation<Int, Never>, Int) -> ()
  // CHECK: [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] {{.*}}, invoke [[BLOCK_IMPL]]
  // CHECK: apply [[METHOD]]([[ARG]], [[BLOCK]], %0)
  // CHECK: [[COPY:%.*]] = copy_value [[ARG]]
  // CHECK: destroy_value [[ARG]]
  // CHECK: await_async_continuation [[CONT]] {{.*}}, resume [[RESUME:bb[0-9]+]]
  // CHECK: [[RESUME]]:
  // CHECK: [[RESULT:%.*]] = load [trivial] [[RESUME_BUF]]
  // CHECK: fix_lifetime [[COPY]]
  // CHECK: destroy_value [[COPY]]
  // CHECK: dealloc_stack [[RESUME_BUF]]
  let _: Int = await slowServer.doSomethingSlow("mail")

  let _: Int = await slowServer.doSomethingSlowNullably("mail")

  // CHECK: [[RESUME_BUF:%.*]] = alloc_stack $String
  // CHECK: [[METHOD:%.*]] = objc_method {{.*}} $@convention(objc_method) (@convention(block) (Optional<NSString>, Optional<NSError>) -> (), SlowServer) -> ()
  // CHECK: [[CONT:%.*]] = get_async_continuation_addr [throws] String, [[RESUME_BUF]]
  // CHECK: [[WRAPPED:%.*]] = struct $UnsafeContinuation<String, Error> ([[CONT]] : $Builtin.RawUnsafeContinuation)
  // CHECK: [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage UnsafeContinuation<String, Error>
  // CHECK: [[CONT_SLOT:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK: store [[WRAPPED]] to [trivial] [[CONT_SLOT]]
  // CHECK: [[BLOCK_IMPL:%.*]] = function_ref @[[STRING_COMPLETION_THROW_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage UnsafeContinuation<String, Error>, Optional<NSString>, Optional<NSError>) -> ()
  // CHECK: [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] {{.*}}, invoke [[BLOCK_IMPL]]
  // CHECK: apply [[METHOD]]([[BLOCK]], %0)
  // CHECK: await_async_continuation [[CONT]] {{.*}}, resume [[RESUME:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
  // CHECK: [[RESUME]]:
  // CHECK: [[RESULT:%.*]] = load [take] [[RESUME_BUF]]
  // CHECK: destroy_value [[RESULT]]
  // CHECK: dealloc_stack [[RESUME_BUF]]
  let _: String = try await slowServer.findAnswer()

  // CHECK: objc_method {{.*}} $@convention(objc_method) (NSString, @convention(block) () -> (), SlowServer) -> ()
  // CHECK: [[BLOCK_IMPL:%.*]] = function_ref @[[VOID_COMPLETION_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage UnsafeContinuation<(), Never>) -> ()
  await slowServer.serverRestart("somewhere")

  // CHECK: function_ref @[[STRING_NONZERO_FLAG_THROW_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage UnsafeContinuation<String, Error>, {{.*}}Bool, Optional<NSString>, Optional<NSError>) -> ()
  let _: String = try await slowServer.doSomethingFlaggy()
  // CHECK: function_ref @[[STRING_ZERO_FLAG_THROW_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage UnsafeContinuation<String, Error>, Optional<NSString>, {{.*}}Bool, Optional<NSError>) -> ()
  let _: String = try await slowServer.doSomethingZeroFlaggy()
  // CHECK: function_ref @[[STRING_STRING_ZERO_FLAG_THROW_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage UnsafeContinuation<(String, String), Error>, {{.*}}Bool, Optional<NSString>, Optional<NSError>, Optional<NSString>) -> ()
  let _: (String, String) = try await slowServer.doSomethingMultiResultFlaggy()

  // CHECK: [[BLOCK_IMPL:%.*]] = function_ref @[[NSSTRING_INT_THROW_COMPLETION_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage UnsafeContinuation<(String, Int), Error>, Optional<NSString>, Int, Optional<NSError>) -> ()
  let (_, _): (String, Int) = try await slowServer.findMultipleAnswers()

  let (_, _): (Bool, Bool) = try await slowServer.findDifferentlyFlavoredBooleans()

  // CHECK: [[ERROR]]([[ERROR_VALUE:%.*]] : @owned $Error):
  // CHECK:   dealloc_stack [[RESUME_BUF]]
  // CHECK:   br [[THROWBB:bb[0-9]+]]([[ERROR_VALUE]]
  // CHECK: [[THROWBB]]([[ERROR_VALUE:%.*]] : @owned $Error):
  // CHECK:   throw [[ERROR_VALUE]]

  let _: String = await slowServer.findAnswerNullably("foo")
  let _: String = try await slowServer.doSomethingDangerousNullably("foo")

  let _: NSObject? = try await slowServer.stopRecording()
  let _: NSObject = try await slowServer.someObject()

  let _: () -> Void = await slowServer.performVoid2Void()
  let _: (Any) -> Void = await slowServer.performId2Void()
  let _: (Any) -> Any = await slowServer.performId2Id()
  let _: (String) -> String = await slowServer.performNSString2NSString()

  let _: ((String) -> String, String) = await slowServer.performNSString2NSStringNSString()
  let _: ((Any) -> Void, (Any) -> Void) = await slowServer.performId2VoidId2Void()
}

func testGeneric<T: AnyObject>(x: GenericObject<T>) async throws {
  let _: T? = try await x.doSomething()
  let _: GenericObject<T>? = await x.doAnotherThing()
}

func testGeneric2<T: AnyObject, U>(x: GenericObject<T>, y: U) async throws {
  let _: T? = try await x.doSomething()
  let _: GenericObject<T>? = await x.doAnotherThing()
}

// CHECK: sil{{.*}}@[[INT_COMPLETION_BLOCK]]
// CHECK:   [[CONT_ADDR:%.*]] = project_block_storage %0
// CHECK:   [[CONT:%.*]] = load [trivial] [[CONT_ADDR]]
// CHECK:   [[RESULT_BUF:%.*]] = alloc_stack $Int
// CHECK:   store %1 to [trivial] [[RESULT_BUF]]
// CHECK:   [[RESUME:%.*]] = function_ref @{{.*}}resumeUnsafeContinuation
// CHECK:   apply [[RESUME]]<Int>([[CONT]], [[RESULT_BUF]])

// CHECK: sil{{.*}}@[[STRING_COMPLETION_THROW_BLOCK]]
// CHECK:   [[RESUME_IN:%.*]] = copy_value %1
// CHECK:   [[ERROR_IN:%.*]] = copy_value %2
// CHECK:   [[CONT_ADDR:%.*]] = project_block_storage %0
// CHECK:   [[CONT:%.*]] = load [trivial] [[CONT_ADDR]]
// CHECK:   [[ERROR_IN_B:%.*]] = begin_borrow [[ERROR_IN]]
// CHECK:   switch_enum [[ERROR_IN_B]] : {{.*}}, case #Optional.some!enumelt: [[ERROR_BB:bb[0-9]+]], case #Optional.none!enumelt: [[RESUME_BB:bb[0-9]+]]
// CHECK: [[RESUME_BB]]:
// CHECK:   [[RESULT_BUF:%.*]] = alloc_stack $String
// CHECK:   [[RESUME_CP:%.*]] = copy_value [[RESUME_IN]]
// CHECK:   [[BRIDGE:%.*]] = function_ref @{{.*}}unconditionallyBridgeFromObjectiveC
// CHECK:   [[BRIDGED_RESULT:%.*]] = apply [[BRIDGE]]([[RESUME_CP]]
// CHECK:   store [[BRIDGED_RESULT]] to [init] [[RESULT_BUF]]
// CHECK:   [[RESUME:%.*]] = function_ref @{{.*}}resumeUnsafeThrowingContinuation
// CHECK:   apply [[RESUME]]<String>([[CONT]], [[RESULT_BUF]])
// CHECK:   br [[END_BB:bb[0-9]+]]
// CHECK: [[END_BB]]:
// CHECK:   return
// CHECK: [[ERROR_BB]]([[ERROR_IN_UNWRAPPED:%.*]] : @guaranteed $NSError):
// CHECK:   [[ERROR:%.*]] = init_existential_ref [[ERROR_IN_UNWRAPPED]]
// CHECK:   [[RESUME_WITH_ERROR:%.*]] = function_ref @{{.*}}resumeUnsafeThrowingContinuationWithError
// CHECK:   [[ERROR_COPY:%.*]] = copy_value [[ERROR]]
// CHECK:   apply [[RESUME_WITH_ERROR]]<String>([[CONT]], [[ERROR_COPY]])
// CHECK:   br [[END_BB]]

// CHECK: sil {{.*}} @[[VOID_COMPLETION_BLOCK]]
// CHECK:   [[CONT_ADDR:%.*]] = project_block_storage %0
// CHECK:   [[CONT:%.*]] = load [trivial] [[CONT_ADDR]]
// CHECK:   [[RESULT_BUF:%.*]] = alloc_stack $()
// CHECK:   [[RESUME:%.*]] = function_ref @{{.*}}resumeUnsafeContinuation
// CHECK:   apply [[RESUME]]<()>([[CONT]], [[RESULT_BUF]])

// CHECK: sil{{.*}}@[[STRING_NONZERO_FLAG_THROW_BLOCK]]
// CHECK:   [[ZERO:%.*]] = integer_literal {{.*}}, 0
// CHECK:   switch_value {{.*}}, case [[ZERO]]: [[ZERO_BB:bb[0-9]+]], default [[NONZERO_BB:bb[0-9]+]]
// CHECK: [[ZERO_BB]]:
// CHECK:   function_ref{{.*}}33_resumeUnsafeThrowingContinuation
// CHECK: [[NONZERO_BB]]:
// CHECK:   function_ref{{.*}}42_resumeUnsafeThrowingContinuationWithError

// CHECK: sil{{.*}}@[[STRING_ZERO_FLAG_THROW_BLOCK]]
// CHECK:   [[ZERO:%.*]] = integer_literal {{.*}}, 0
// CHECK:   switch_value {{.*}}, case [[ZERO]]: [[ZERO_BB:bb[0-9]+]], default [[NONZERO_BB:bb[0-9]+]]
// CHECK: [[NONZERO_BB]]:
// CHECK:   function_ref{{.*}}33_resumeUnsafeThrowingContinuation
// CHECK: [[ZERO_BB]]:
// CHECK:   function_ref{{.*}}42_resumeUnsafeThrowingContinuationWithError

// CHECK: sil{{.*}}@[[STRING_STRING_ZERO_FLAG_THROW_BLOCK]]
// CHECK:   [[ZERO:%.*]] = integer_literal {{.*}}, 0
// CHECK:   switch_value {{.*}}, case [[ZERO]]: [[ZERO_BB:bb[0-9]+]], default [[NONZERO_BB:bb[0-9]+]]
// CHECK: [[NONZERO_BB]]:
// CHECK:   function_ref{{.*}}33_resumeUnsafeThrowingContinuation
// CHECK: [[ZERO_BB]]:
// CHECK:   function_ref{{.*}}42_resumeUnsafeThrowingContinuationWithError

// CHECK: sil{{.*}}@[[NSSTRING_INT_THROW_COMPLETION_BLOCK]]
// CHECK:   [[RESULT_BUF:%.*]] = alloc_stack $(String, Int)
// CHECK:   [[RESULT_0_BUF:%.*]] = tuple_element_addr [[RESULT_BUF]] {{.*}}, 0
// CHECK:   [[BRIDGE:%.*]] = function_ref @{{.*}}unconditionallyBridgeFromObjectiveC
// CHECK:   [[BRIDGED:%.*]] = apply [[BRIDGE]]
// CHECK:   store [[BRIDGED]] to [init] [[RESULT_0_BUF]]
// CHECK:   [[RESULT_1_BUF:%.*]] = tuple_element_addr [[RESULT_BUF]] {{.*}}, 1
// CHECK:   store %2 to [trivial] [[RESULT_1_BUF]]

