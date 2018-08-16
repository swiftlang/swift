// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest


import GameplayKit

var GameplayKitTests = TestSuite("GameplayKit")

if #available(OSX 10.12, iOS 10.0, tvOS 10.0, *) {

  // MARK: Doing GameplayKit Stuff

  GameplayKitTests.test("GKPath_float2") {
    var vec: [float2] = [float2(3.0), float2(4.0)]
    let path = GKPath(points: vec, radius: Float(30), cyclical: true)
    expectEqual(path.numPoints, 2)
    expectEqual(path.radius, Float(30))
    expectEqual(path.isCyclical, true)
  }
  GameplayKitTests.test("GKPath_float3") {
    var vec: [float3] = [float3(3.0), float3(4.0)]
    let path = GKPath(points: vec, radius: Float(30), cyclical: true)
    expectEqual(path.numPoints, 2)
    expectEqual(path.radius, Float(30))
    expectEqual(path.isCyclical, true)
  }
  GameplayKitTests.test("GKPolygonObstacle") {
    var vec = [float2(3.0, 3.0), float2(3.0, -3.0), float2(-3.0, 3.0), float2(-3.0, -3.0)]
    let obstacle = GKPolygonObstacle(points: vec)
    expectEqual(obstacle.vertexCount, 4)
  }
  GameplayKitTests.test("GKEntity") {
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

runAllTests()
