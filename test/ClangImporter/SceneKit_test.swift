// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SceneKit
import Foundation

// Test out some explicit renames of typedefs and globals, which are now new
// wrapper types with nestest values.
@available(macOS 10.11, *)
func testNestingRenames() {
  let _ = SCNGeometrySourceSemantic
    // expected-error@-1{{'SCNGeometrySourceSemantic' has been renamed to 'SCNGeometrySource.Semantic'}}
  let _ = SCNLightType
    // expected-error@-1{{'SCNLightType' has been renamed to 'SCNLight.LightType'}}
  let _ = SCNLightingModel
    // expected-error@-1{{'SCNLightingModel' has been renamed to 'SCNMaterial.LightingModel'}}
  let _ = SCNParticleProperty
    // expected-error@-1{{'SCNParticleProperty' has been renamed to 'SCNParticleSystem.ParticleProperty'}}
  let _ = SCNPhysicsShapeOption
    // expected-error@-1{{'SCNPhysicsShapeOption' has been renamed to 'SCNPhysicsShape.Option'}}
  let _ = SCNPhysicsShapeType
    // expected-error@-1{{'SCNPhysicsShapeType' has been renamed to 'SCNPhysicsShape.ShapeType'}}
  let _ = SCNPhysicsTestOption
    // expected-error@-1{{'SCNPhysicsTestOption' has been renamed to 'SCNPhysicsWorld.TestOption'}}
  let _ = SCNPhysicsTestSearchMode
    // expected-error@-1{{'SCNPhysicsTestSearchMode' has been renamed to 'SCNPhysicsWorld.TestSearchMode'}}
  let _ = SCNSceneAttribute
    // expected-error@-1{{'SCNSceneAttribute' has been renamed to 'SCNScene.Attribute'}}
  let _ = SCNSceneSourceAnimationImportPolicy
    // expected-error@-1{{'SCNSceneSourceAnimationImportPolicy' has been renamed to 'SCNSceneSource.AnimationImportPolicy'}}
  let _ = SCNSceneSourceLoadingOption
    // expected-error@-1{{'SCNSceneSourceLoadingOption' has been renamed to 'SCNSceneSource.LoadingOption'}}
  let _ = SCNViewOption
    // expected-error@-1{{'SCNViewOption' has been renamed to 'SCNView.Option'}}
  let _ = SCNHitTestFirstFoundOnlyKey
    // expected-error@-1{{'SCNHitTestFirstFoundOnlyKey' has been renamed to 'SCNHitTestOption.firstFoundOnly'}}
  let _ = SCNHitTestSortResultsKey
    // expected-error@-1{{'SCNHitTestSortResultsKey' has been renamed to 'SCNHitTestOption.sortResults'}}
  let _ = SCNHitTestClipToZRangeKey
    // expected-error@-1{{'SCNHitTestClipToZRangeKey' has been renamed to 'SCNHitTestOption.clipToZRange'}}
  let _ = SCNHitTestBackFaceCullingKey
    // expected-error@-1{{'SCNHitTestBackFaceCullingKey' has been renamed to 'SCNHitTestOption.backFaceCulling'}}
  let _ = SCNHitTestBoundingBoxOnlyKey
    // expected-error@-1{{'SCNHitTestBoundingBoxOnlyKey' has been renamed to 'SCNHitTestOption.boundingBoxOnly'}}
  let _ = SCNHitTestIgnoreChildNodesKey
    // expected-error@-1{{'SCNHitTestIgnoreChildNodesKey' has been renamed to 'SCNHitTestOption.ignoreChildNodes'}}
  let _ = SCNHitTestRootNodeKey
    // expected-error@-1{{'SCNHitTestRootNodeKey' has been renamed to 'SCNHitTestOption.rootNode'}}
  let _ = SCNHitTestIgnoreHiddenNodesKey
    // expected-error@-1{{'SCNHitTestIgnoreHiddenNodesKey' has been renamed to 'SCNHitTestOption.ignoreHiddenNodes'}}
  let _ = SCNPhysicsShapeTypeKey
    // expected-error@-1{{'SCNPhysicsShapeTypeKey' has been renamed to 'SCNPhysicsShape.Option.type'}}
  let _ = SCNPhysicsShapeKeepAsCompoundKey
    // expected-error@-1{{'SCNPhysicsShapeKeepAsCompoundKey' has been renamed to 'SCNPhysicsShape.Option.keepAsCompound'}}
  let _ = SCNPhysicsShapeScaleKey
    // expected-error@-1{{'SCNPhysicsShapeScaleKey' has been renamed to 'SCNPhysicsShape.Option.scale'}}
  let _ = SCNPhysicsTestCollisionBitMaskKey
    // expected-error@-1{{'SCNPhysicsTestCollisionBitMaskKey' has been renamed to 'SCNPhysicsWorld.TestOption.collisionBitMask'}}
  let _ = SCNPhysicsTestSearchModeKey
    // expected-error@-1{{'SCNPhysicsTestSearchModeKey' has been renamed to 'SCNPhysicsWorld.TestOption.searchMode'}}
  let _ = SCNPhysicsTestBackfaceCullingKey
    // expected-error@-1{{'SCNPhysicsTestBackfaceCullingKey' has been renamed to 'SCNPhysicsWorld.TestOption.backfaceCulling'}}
  let _ = SCNSceneStartTimeAttributeKey
    // expected-error@-1{{'SCNSceneStartTimeAttributeKey' has been renamed to 'SCNScene.Attribute.startTime'}}
  let _ = SCNSceneEndTimeAttributeKey
    // expected-error@-1{{'SCNSceneEndTimeAttributeKey' has been renamed to 'SCNScene.Attribute.endTime'}}
  let _ = SCNSceneFrameRateAttributeKey
    // expected-error@-1{{'SCNSceneFrameRateAttributeKey' has been renamed to 'SCNScene.Attribute.frameRate'}}
  let _ = SCNSceneUpAxisAttributeKey
    // expected-error@-1{{'SCNSceneUpAxisAttributeKey' has been renamed to 'SCNScene.Attribute.upAxis'}}
  let _ = SCNSceneSourceCreateNormalsIfAbsentKey
    // expected-error@-1{{'SCNSceneSourceCreateNormalsIfAbsentKey' has been renamed to 'SCNSceneSource.LoadingOption.createNormalsIfAbsent'}}
  let _ = SCNSceneSourceCheckConsistencyKey
    // expected-error@-1{{'SCNSceneSourceCheckConsistencyKey' has been renamed to 'SCNSceneSource.LoadingOption.checkConsistency'}}
  let _ = SCNSceneSourceFlattenSceneKey
    // expected-error@-1{{'SCNSceneSourceFlattenSceneKey' has been renamed to 'SCNSceneSource.LoadingOption.flattenScene'}}
  let _ = SCNSceneSourceUseSafeModeKey
    // expected-error@-1{{'SCNSceneSourceUseSafeModeKey' has been renamed to 'SCNSceneSource.LoadingOption.useSafeMode'}}
  let _ = SCNSceneSourceAssetDirectoryURLsKey
    // expected-error@-1{{'SCNSceneSourceAssetDirectoryURLsKey' has been renamed to 'SCNSceneSource.LoadingOption.assetDirectoryURLs'}}
  let _ = SCNSceneSourceOverrideAssetURLsKey
    // expected-error@-1{{'SCNSceneSourceOverrideAssetURLsKey' has been renamed to 'SCNSceneSource.LoadingOption.overrideAssetURLs'}}
  let _ = SCNSceneSourceStrictConformanceKey
    // expected-error@-1{{'SCNSceneSourceStrictConformanceKey' has been renamed to 'SCNSceneSource.LoadingOption.strictConformance'}}
  let _ = SCNSceneSourceConvertUnitsToMetersKey
    // expected-error@-1{{'SCNSceneSourceConvertUnitsToMetersKey' has been renamed to 'SCNSceneSource.LoadingOption.convertUnitsToMeters'}}
  let _ = SCNSceneSourceConvertToYUpKey
    // expected-error@-1{{'SCNSceneSourceConvertToYUpKey' has been renamed to 'SCNSceneSource.LoadingOption.convertToYUp'}}
  let _ = SCNSceneSourceAnimationImportPolicyKey
    // expected-error@-1{{'SCNSceneSourceAnimationImportPolicyKey' has been renamed to 'SCNSceneSource.LoadingOption.animationImportPolicy'}}
  let _ = SCNPreferredRenderingAPIKey
    // expected-error@-1{{'SCNPreferredRenderingAPIKey' has been renamed to 'SCNView.Option.preferredRenderingAPI'}}
  let _ = SCNPreferredDeviceKey
    // expected-error@-1{{'SCNPreferredDeviceKey' has been renamed to 'SCNView.Option.preferredDevice'}}
  let _ = SCNPreferLowPowerDeviceKey
    // expected-error@-1{{'SCNPreferLowPowerDeviceKey' has been renamed to 'SCNView.Option.preferLowPowerDevice'}}
}

// All OK
@available(macOS 10.11, *)
func useRenamedValues() {
  let _ = SCNGeometrySource.Semantic.self
  let _ = SCNLight.LightType.self
  let _ = SCNMaterial.LightingModel.self
  let _ = SCNParticleSystem.ParticleProperty.self
  let _ = SCNPhysicsShape.Option.self
  let _ = SCNPhysicsShape.ShapeType.self
  let _ = SCNPhysicsWorld.TestOption.self
  let _ = SCNPhysicsWorld.TestSearchMode.self
  let _ = SCNScene.Attribute.self
  let _ = SCNSceneSource.AnimationImportPolicy.self
  let _ = SCNSceneSource.LoadingOption.self
  let _ = SCNView.Option.self
  let _ = SCNHitTestOption.firstFoundOnly
  let _ = SCNHitTestOption.sortResults
  let _ = SCNHitTestOption.clipToZRange
  let _ = SCNHitTestOption.backFaceCulling
  let _ = SCNHitTestOption.boundingBoxOnly
  let _ = SCNHitTestOption.ignoreChildNodes
  let _ = SCNHitTestOption.rootNode
  let _ = SCNHitTestOption.ignoreHiddenNodes
  let _ = SCNPhysicsShape.Option.type
  let _ = SCNPhysicsShape.Option.keepAsCompound
  let _ = SCNPhysicsShape.Option.scale
  let _ = SCNPhysicsWorld.TestOption.collisionBitMask
  let _ = SCNPhysicsWorld.TestOption.searchMode
  let _ = SCNPhysicsWorld.TestOption.backfaceCulling
  let _ = SCNScene.Attribute.startTime
  let _ = SCNScene.Attribute.endTime
  let _ = SCNScene.Attribute.frameRate
  let _ = SCNScene.Attribute.upAxis
  let _ = SCNSceneSource.LoadingOption.createNormalsIfAbsent
  let _ = SCNSceneSource.LoadingOption.checkConsistency
  let _ = SCNSceneSource.LoadingOption.flattenScene
  let _ = SCNSceneSource.LoadingOption.useSafeMode
  let _ = SCNSceneSource.LoadingOption.assetDirectoryURLs
  let _ = SCNSceneSource.LoadingOption.overrideAssetURLs
  let _ = SCNSceneSource.LoadingOption.strictConformance
  let _ = SCNSceneSource.LoadingOption.convertUnitsToMeters
  let _ = SCNSceneSource.LoadingOption.convertToYUp
  let _ = SCNSceneSource.LoadingOption.animationImportPolicy
  let _ = SCNView.Option.preferredRenderingAPI
  let _ = SCNView.Option.preferredDevice
  let _ = SCNView.Option.preferLowPowerDevice
}

// All OK
@available(macOS 10.12, *)
func useRenamedAPIs(actionable: SCNActionable, action: SCNAction, data: Data,
                    timeInterval: TimeInterval, vec3: SCNVector3, node: SCNNode,
                    audioSource: SCNAudioSource, animatable: SCNAnimatable,
                    lookAtConstraint: SCNLookAtConstraint, mat4: SCNMatrix4,
                    particleSystem: SCNParticleSystem, event: SCNParticleEvent,
                    stage: SCNParticleModifierStage, animation: CAAnimation,
                    bindingBlock: @escaping SCNBindingBlock, material: SCNMaterial,
                    bufferBindingBlock: @escaping SCNBufferBindingBlock, vec4: SCNVector4,
                    eventBlock: @escaping SCNParticleEventBlock, morpher: SCNMorpher,
                    modifierBlock: @escaping SCNParticleModifierBlock, scene: SCNScene,
                    physicsBehavior: SCNPhysicsBehavior, geometry: SCNGeometry,
                    physicsBody: SCNPhysicsBody, sceneSource: SCNSceneSource,
                    physicsWorld: SCNPhysicsWorld, point: CGPoint,
                    physicsShape: SCNPhysicsShape, shadable: SCNShadable,
                    voidPtr: UnsafeMutableRawPointer, program: SCNProgram,
                    renderer: SCNSceneRenderer, bufferStream: SCNBufferStream,
                    bufferFrequency: SCNBufferFrequency,
                    semantic: SCNGeometrySource.Semantic,
                    prop: SCNParticleSystem.ParticleProperty) {
  actionable.runAction(action)
  actionable.runAction(action, completionHandler: {})
  actionable.runAction(action, forKey: "key", completionHandler: {})

  let _ = SCNAction.rotateTo(x: 1.0, y: 2.0, z: 3.0, duration: timeInterval,
                             usesShortestUnitArc: false)
  let _ = SCNAction.rotate(by: 1.0, around: vec3, duration: timeInterval)
  let _ = SCNAction.fadeIn(duration: timeInterval)
  let _ = SCNAction.fadeOut(duration: timeInterval)
  let _ = SCNAction.wait(duration: timeInterval)
  let _ = SCNAction.wait(duration: timeInterval, withRange: timeInterval)
  let _ = SCNAction.customAction(duration: timeInterval,
                                 action: { (a, b) in () })
  let _ = SCNAction.playAudio(audioSource, waitForCompletion: false)

  animatable.addAnimation(animation, forKey: "key")
  let _ = animatable.isAnimationPaused(forKey: "key")
  let _ = animatable.setAnimationSpeed(1.0, forKey: "key")

  let _ = lookAtConstraint.isGimbalLockEnabled
  let _ = SCNIKConstraint.inverseKinematicsConstraint(chainRootNode: node)

  let _ = geometry.material(named: "mat")
  let _ = geometry.sources(for: semantic)
  let geoSrc = SCNGeometrySource(data: data, semantic: semantic, vectorCount: 2,
                                 usesFloatComponents: false,
                                 componentsPerVector: 3,
                                 bytesPerComponent: 11, dataOffset: -2,
                                 dataStride: -3)
  let _ = geoSrc.usesFloatComponents

  let _ = material.lightingModel
  let _ = morpher.weight(forTargetAt: 1)
  let _ = node.hitTestWithSegment(from: vec3, to: vec3, options: [:])

  let _ = particleSystem.isAffectedByGravity
  let _ = particleSystem.isAffectedByPhysicsFields
  particleSystem.handle(event, forProperties: [prop], handler: eventBlock)
  particleSystem.addModifier(forProperties: [prop], at: stage,
                             modifier: modifierBlock)
  particleSystem.removeModifiers(at: stage)
  scene.addParticleSystem(particleSystem, transform: mat4)

  physicsBody.applyForce(vec3, asImpulse: false)
  physicsBody.applyForce(vec3, at: vec3, asImpulse: false)
  physicsBody.applyTorque(vec4, asImpulse: false)
  let _ = SCNPhysicsField.noiseField(smoothness: 1.0, animationSpeed: 1.0)
  let _ = SCNPhysicsField.turbulenceField(smoothness: 1.0, animationSpeed: 1.0)
  physicsWorld.addBehavior(physicsBehavior)
  physicsWorld.removeBehavior(physicsBehavior)
  let _ = physicsWorld.rayTestWithSegment(from: vec3, to: vec3, options: [:])
  let _ = physicsWorld.contactTestBetween(physicsBody, physicsBody,
                                          options: [:])
  let _ = physicsWorld.contactTest(with: physicsBody, options: [:])
  let _ = physicsWorld.convexSweepTest(with: physicsShape, from: mat4, to: mat4)

  let _ = renderer.isNode(node, insideFrustumOf: node)
  let _ = renderer.nodesInsideFrustum(of: node)
  renderer.prepare([], completionHandler: { b in ()})
  let _ = sceneSource.identifiersOfEntries(withClass: SCNNode.self)

  bufferStream.writeBytes(voidPtr, count: 2)

  shadable.handleBinding!(ofSymbol: "sym", handler: bindingBlock)
  shadable.handleUnbinding!(ofSymbol: "sym", handler: bindingBlock)
  program.handleBinding(ofBufferNamed: "str", frequency: bufferFrequency,
                        handler: bufferBindingBlock)
}
