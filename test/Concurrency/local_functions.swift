// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o - -verify -strict-concurrency=complete -enable-actor-data-race-checks -disable-availability-checking | %FileCheck %s

// Issue #80772. This used to crash in SILGen because we gave local functions
// the isolation of their enclosing context instead of trying to convert
// parameter isolation to capture isolation.
actor TestActor {
  // CHECK-LABEL: // nested #1 () in TestActor.testWithoutCapture()
  // CHECK-NEXT:  // Isolation: actor_instance. name: 'self'
  func testWithoutCapture() {
    func nested() -> String {
      return "test"
    }

    print(nested())
  }

  // CHECK-LABEL: // nested #1 () in TestActor.testWithCapture()
  // CHECK-NEXT:  // Isolation: actor_instance. name: 'self'
  // CHECK:       [[SELF_EXECUTOR:%.*]] = extract_executor %0
  // CHECK:       [[CHECK_FN:%.*]] = function_ref @swift_task_isCurrentExecutor
  // CHECK:       apply [[CHECK_FN]]([[SELF_EXECUTOR]])
  func testWithCapture() {
    func nested() -> String {
      _ = self
      return "test"
    }

    print(nested())
  }
}

@globalActor struct GenericGlobalActor<T> {
  static var shared: TestActor {
    // not a valid implementation
    return TestActor()
  }
}

struct Generic<T> {
  // CHECK-LABEL: // nested #1 <A><A1>(_:) in Generic.testGenericGlobalActor()
  // CHECK-NEXT:  // Isolation: global_actor. type: GenericGlobalActor<T>
  @GenericGlobalActor<T> func testGenericGlobalActor() {
    func nested<U>(_ type: U.Type) -> String {
      // CHECK: [[FN:%.*]] = function_ref @$s15local_functions18GenericGlobalActorV6sharedAA04TestE0CvgZ
      // CHECK: apply [[FN]]<T>(
      return "test"
    }

    print(nested(Int.self))
  }
}

actor MyActor {
  // CHECK-LABEL: // nested #1 () in MyActor.deinit
  // CHECK-NEXT:  // Isolation: actor_instance. name: 'self'
  isolated deinit {
    func nested() -> String {
      return "test"
    }
    print(nested())
  }
}
