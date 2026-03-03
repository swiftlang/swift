// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple %s
// RUN: %target-swift-frontend -emit-sil -target %target-swift-5.7-abi-triple -DMAKE_CORRECT %s -o - | %FileCheck %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

distributed actor Foo {
  distributed func alwaysThrows() throws(FooError) { }

  func alwaysPropagates() throws(FooError) {
    // okay, produces FooError
    try alwaysThrows()
    _ = try value
  }

  distributed var value: String {
    get throws(FooError) {
      throw FooError()
    }
  }
}

struct FooError: Codable, Error { }
struct RemoteInvocationError: Codable, Error { }

#if !MAKE_CORRECT
func testBad(foo: Foo) async throws(FooError) {
  try await foo.alwaysThrows() // expected-error{{thrown expression type 'any Error' cannot be converted to error type 'FooError'}}

  _ = try await foo.value // expected-error{{thrown expression type 'any Error' cannot be converted to error type 'FooError'}}
}

func testBadDoCatch(foo: Foo) async throws {
  do {
    try await foo.alwaysThrows()
    _ = try await foo.value
  } catch let error {
    let _: Int = error // expected-error{{cannot convert value of type 'any Error' to specified type 'Int'}}
  }
}
#endif

// Distributed thunk for calling alwaysThrows() handles the translation.
// CHECK-LABEL: sil hidden [thunk] [distributed] [ref_adhoc_requirement_witness "$s11Distributed29LocalTestingInvocationDecoderC18decodeNextArgumentxyKSeRzSERzlF"] @$s30distributed_actor_typed_throws3FooC12alwaysThrowsyyYaKFTE
// CHECK: [[LOCAL_FN:%.*]] = function_ref @$s30distributed_actor_typed_throws3FooC12alwaysThrowsyyAA0E5ErrorVYKF : $@convention(method) (@sil_isolated @guaranteed Foo) -> @error FooError
// CHECK-NEXT: hop_to_executor [[FOO:%[0-9]+]]
// CHECK-NEXT: try_apply [[LOCAL_FN]]([[FOO]]) : $@convention(method) (@sil_isolated @guaranteed Foo) -> @error FooError, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
// CHECK: [[ERROR_BB]]([[FOO_ERROR:%.*]] : $FooError):
// CHECK: alloc_existential_box $any Error, $FooError
// CHECK-NEXT: project_existential_box $FooError
// CHECK: store [[FOO_ERROR]]

// CHECK-LABEL: sil hidden @$s30distributed_actor_typed_throws8testGood3fooyAA3FooC_tYaKF : $@convention(thin) @async (@guaranteed Foo) -> @error any Error
func testGood(foo: Foo) async throws {
  // CHECK: function_ref @$s30distributed_actor_typed_throws3FooC12alwaysThrowsyyYaKFTE : $@convention(method) @async (@guaranteed Foo) -> @error any Error
  try await foo.alwaysThrows()

  _ = try await foo.value
}

func testDoCatch(foo: Foo) async throws(FooError) {
  do {
    try await foo.alwaysThrows()
    _ = try await foo.value
  } catch let error {
    if let fe = error as? FooError {
      throw fe
    }
  }
}
