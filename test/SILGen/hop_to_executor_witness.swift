// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name test -swift-version 5 | %FileCheck --enable-var-scope %s --implicit-check-not 'hop_to_executor {{%[0-9]+}}'
// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
protocol TestableAsync {
  func test() async
}

@available(SwiftStdlib 5.1, *)
actor TestActor : TestableAsync {
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4test9TestActorCAA13TestableAsyncA2aDPAAyyYaFTW : $@convention(witness_method: TestableAsync) @async (@in_guaranteed TestActor) -> ()
  // CHECK: [[ACTOR_INSTANCE:%.*]] = load_borrow %0 : $*TestActor
  // CHECK-NEXT:  hop_to_executor [[ACTOR_INSTANCE]] : $TestActor
  func test() { }
}

@available(SwiftStdlib 5.1, *)
@MainActor class TestClass : TestableAsync {
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4test9TestClassCAA13TestableAsyncA2aDPAAyyYaFTW : $@convention(witness_method: TestableAsync) @async (@in_guaranteed TestClass) -> ()
  // CHECK: hop_to_executor {{%.*}} : $MainActor 
  func test() { }
}
