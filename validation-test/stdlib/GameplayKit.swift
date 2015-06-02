// RUN: %target-run-simple-swift

// REQUIRES: objc_interop

// REQUIRES: GameplayKit
// FIXME: There should be no extra requirements in this test.
// The availability check below is not enough because of:
// <rdar://problem/20924154> No backward deployment story for overlays for new frameworks

import StdlibUnittest
import GameplayKit

// GameplayKit is only available on iOS 9.0 and above and on OS X 10.11
// and above.

var GamePlayKitTests = TestSuite("GameplayKit")

if #available(OSX 10.11, iOS 9.0, *) {

class TestComponent : GKComponent {}
class OtherTestComponent : GKComponent {}

class TestState1 : GKState {}
class TestState2 : GKState {}

GamePlayKitTests.test("GKEntity.componentForClass()") {
  let entity = GKEntity()
  entity.addComponent(TestComponent())

  if true {
    var componentForTestComponent =
      entity.componentForClass(TestComponent.self)
    var componentForOtherTestComponent_nil =
      entity.componentForClass(OtherTestComponent.self)

    expectNotEmpty(componentForTestComponent)
    expectType(Optional<TestComponent>.self, &componentForTestComponent)

    expectEmpty(componentForOtherTestComponent_nil)
  }

  entity.removeComponentForClass(TestComponent.self)
  entity.addComponent(OtherTestComponent())

  if true {
    var componentForOtherTestComponent =
      entity.componentForClass(OtherTestComponent.self)
    var componentForTestComponent_nil =
      entity.componentForClass(TestComponent.self)

    expectNotEmpty(componentForOtherTestComponent)
    expectType(Optional<OtherTestComponent>.self, &componentForOtherTestComponent)

    expectEmpty(componentForTestComponent_nil)
  }
}

GamePlayKitTests.test("GKStateMachine.stateForClass()") {
  if true {
    // Construct a state machine with a custom subclass as the only state.
    let stateMachine = GKStateMachine(
      states: [TestState1()])

    var stateForTestState1 =
      stateMachine.stateForClass(TestState1.self)
    var stateForTestState2_nil =
      stateMachine.stateForClass(TestState2.self)

    expectNotEmpty(stateForTestState1)
    expectType(Optional<TestState1>.self, &stateForTestState1)

    expectEmpty(stateForTestState2_nil)
  }

  if true {
    // Construct a state machine with a custom subclass as the only state.
    let stateMachine = GKStateMachine(
      states: [TestState2()])

    var stateForTestState2 =
      stateMachine.stateForClass(TestState2.self)
    var stateForTestState1_nil =
      stateMachine.stateForClass(TestState1.self)

    expectNotEmpty(stateForTestState2)
    expectType(Optional<TestState2>.self, &stateForTestState2)

    expectEmpty(stateForTestState1_nil)
  }
}

} // if #available(OSX 10.11, iOS 9.0, *)

runAllTests()

