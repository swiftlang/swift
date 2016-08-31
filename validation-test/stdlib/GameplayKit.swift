// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest


import GameplayKit

// GameplayKit is only available on iOS 9.0 and above, OS X 10.11 and above, and
// tvOS 9.0 and above.

var GamePlayKitTests = TestSuite("GameplayKit")

if #available(OSX 10.12, iOS 10.0, tvOS 10.0, *) {

  GamePlayKitTests.test("GKPath_float2") {
    let vec: [float2] = [float2(3.0), float2(4.0)]
    let path = GKPath(points: vec, radius: Float(30), cyclical: true)
    expectEqual(path.numPoints, 2)
    expectEqual(path.radius, Float(30))
    expectEqual(path.isCyclical, true)
  }
  GamePlayKitTests.test("GKPath_float3") {
    let vec: [float3] = [float3(3.0), float3(4.0)]
    let path = GKPath(points: vec, radius: Float(30), cyclical: true)
    expectEqual(path.numPoints, 2)
    expectEqual(path.radius, Float(30))
    expectEqual(path.isCyclical, true)
  }
  GamePlayKitTests.test("GKPolygonObstacle") {
    let vec = [float2(3.0, 3.0), float2(3.0, -3.0), float2(-3.0, 3.0), float2(-3.0, -3.0)]
    let obstacle = GKPolygonObstacle(points: vec)
    expectEqual(obstacle.vertexCount, 4)
  }
  GamePlayKitTests.test("GKEntity") {
    @objc(MovementComponent)
    class MovementComponent: GKComponent {
      override func update(deltaTime seconds: TimeInterval) {}
      override func didAddToEntity() {}
      override func willRemoveFromEntity() {}
    }
    let comp = MovementComponent()
    let entity = GKEntity()
    entity.addComponent(comp)
    expectEqual(entity.components.count, 1)
    let grabbedComp = entity.component(ofType: MovementComponent.self)
    expectEqual(grabbedComp, comp)
    entity.removeComponent(ofType: MovementComponent.self)
    expectEqual(entity.components.count, 0)
  }
}

if #available(OSX 10.11, iOS 9.0, tvOS 9.0, *) {

class TestComponent : GKComponent {}
class OtherTestComponent : GKComponent {}

class TestState1 : GKState {}
class TestState2 : GKState {}

GamePlayKitTests.test("GKEntity.component(ofType)") {
  let entity = GKEntity()
  entity.addComponent(TestComponent())

  do {
    var componentForTestComponent =
      entity.component(ofType: TestComponent.self)
    var componentForOtherTestComponent_nil =
      entity.component(ofType: OtherTestComponent.self)

    expectNotEmpty(componentForTestComponent)
    expectType(Optional<TestComponent>.self, &componentForTestComponent)

    expectEmpty(componentForOtherTestComponent_nil)
  }

  entity.removeComponent(ofType: TestComponent.self)
  entity.addComponent(OtherTestComponent())

  do {
    var componentForOtherTestComponent =
      entity.component(ofType: OtherTestComponent.self)
    var componentForTestComponent_nil =
      entity.component(ofType: TestComponent.self)

    expectNotEmpty(componentForOtherTestComponent)
    expectType(Optional<OtherTestComponent>.self, &componentForOtherTestComponent)

    expectEmpty(componentForTestComponent_nil)
  }
}

GamePlayKitTests.test("GKStateMachine.state(forClass:)") {
  do {
    // Construct a state machine with a custom subclass as the only state.
    let stateMachine = GKStateMachine(
      states: [TestState1()])

    var stateForTestState1 =
      stateMachine.state(forClass: TestState1.self)
    var stateForTestState2_nil =
      stateMachine.state(forClass: TestState2.self)

    expectNotEmpty(stateForTestState1)
    expectType(Optional<TestState1>.self, &stateForTestState1)

    expectEmpty(stateForTestState2_nil)
  }

  do {
    // Construct a state machine with a custom subclass as the only state.
    let stateMachine = GKStateMachine(
      states: [TestState2()])

    var stateForTestState2 =
      stateMachine.state(forClass: TestState2.self)
    var stateForTestState1_nil =
      stateMachine.state(forClass: TestState1.self)

    expectNotEmpty(stateForTestState2)
    expectType(Optional<TestState2>.self, &stateForTestState2)

    expectEmpty(stateForTestState1_nil)
  }
}

} // if #available(OSX 10.11, iOS 9.0, tvOS 9.0, *)

runAllTests()

