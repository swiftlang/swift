// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out4 -swift-version 4 && %target-codesign %t/a.out4 && %target-run %t/a.out4
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

// REQUIRES: executable_test

import StdlibUnittest

import Metal

var MetalTests = TestSuite("Metal")

if #available(OSX 10.13, iOS 11.0, tvOS 11.0, *) {

  // Call each overlay to ensure nothing explodes

  MetalTests.test("MTLArgumentEncoder") {
    func apiAvailabilityTest() {
      /* Setup */

      let device = MTLCreateSystemDefaultDevice()!
      let buf = device.makeBuffer(
        length: 64, options: MTLResourceOptions.storageModeShared)!
      let texDesc = MTLTextureDescriptor()
      texDesc.usage = MTLTextureUsage.renderTarget
      let tex = device.makeTexture(descriptor: texDesc)!
      let smplr = device.makeSamplerState(descriptor: MTLSamplerDescriptor())

      var arguments = [MTLArgumentDescriptor]()
      arguments.append(MTLArgumentDescriptor())
      arguments.append(MTLArgumentDescriptor())
      arguments.append(MTLArgumentDescriptor())
      arguments[0].dataType = MTLDataType.pointer
      arguments[0].index = 0
      arguments[1].dataType = MTLDataType.texture
      arguments[1].index = 1
      arguments[2].dataType = MTLDataType.sampler
      arguments[2].index = 2
      if #available(OSX 10.14, iOS 12.0, tvOS 12.0, *){
        arguments.append(MTLArgumentDescriptor())
        arguments[3].dataType = MTLDataType.indirectCommandBuffer
        arguments[3].index = 3
      }

      /* Call APIs */

      let argEncoder = device.makeArgumentEncoder(arguments: arguments)!
      argEncoder.setArgumentBuffer(buf, offset: 0)
      argEncoder.setBuffers([buf], offsets: [0], range: 0..<1)
      argEncoder.setTextures([tex], range: 1..<2)
      argEncoder.setSamplerStates([smplr], range: 2..<3)

      if #available(OSX 10.14, iOS 12.0, tvOS 12.0, *){
        let icbDesc = MTLIndirectCommandBufferDescriptor()
        icbDesc.commandTypes = [.draw, .drawIndexed]
        icbDesc.inheritBuffers = false
        icbDesc.maxVertexBufferBindCount = 1
        icbDesc.maxFragmentBufferBindCount = 1
        let icb = device.makeIndirectCommandBuffer (descriptor: icbDesc, maxCommandCount: 1, options: MTLResourceOptions.storageModeShared )!
        argEncoder.setIndirectCommandBuffers([icb], range: 3..<4)
      }
    }
  }

  MetalTests.test("MTLBlitCommandEncoder") {
    func apiAvailabilityTest() {

      /* Setup */

      let device = MTLCreateSystemDefaultDevice()!
      let queue = device.makeCommandQueue()!
      let cmdBuf = queue.makeCommandBuffer()!
      let bltCmdEncdr = cmdBuf.makeBlitCommandEncoder()!

      /* Call APIs */

      let buf = device.makeBuffer(length: 4, options: MTLResourceOptions())!
      bltCmdEncdr.fill(buffer: buf, range: 0..<buf.length, value: 0)

      if #available(OSX 10.14, iOS 12.0, tvOS 12.0, *){
        let icbDesc = MTLIndirectCommandBufferDescriptor()
        icbDesc.commandTypes = [.draw, .drawIndexed]
        icbDesc.inheritBuffers = false
        icbDesc.maxVertexBufferBindCount = 1
        icbDesc.maxFragmentBufferBindCount = 1
        let icb1 = device.makeIndirectCommandBuffer (descriptor: icbDesc, maxCommandCount: 4, options: MTLResourceOptions.storageModeShared )!
        let icb2 = device.makeIndirectCommandBuffer (descriptor: icbDesc, maxCommandCount: 4, options: MTLResourceOptions.storageModeShared )!
        bltCmdEncdr.resetCommandsInBuffer (icb1, range:0..<5)
        bltCmdEncdr.resetCommandsInBuffer (icb2, range:0..<5)
        bltCmdEncdr.copyIndirectCommandBuffer (icb1, sourceRange: 0..<5, destination: icb2, destinationIndex:0)
        bltCmdEncdr.optimizeIndirectCommandBuffer (icb1, range:0..<5)
      }

      bltCmdEncdr.endEncoding()
    }
  }

if #available(OSX 10.14, iOS 12.0, tvOS 12.0, *){
  MetalTests.test("MTLIndirectCommandBuffer"){
    func apiAvailabilityTest() {

      /* Setup */

      let device = MTLCreateSystemDefaultDevice()!
      let queue = device.makeCommandQueue()!
      let cmdBuf = queue.makeCommandBuffer()!
      let texDesc = MTLTextureDescriptor()
      texDesc.usage = MTLTextureUsage.renderTarget
      let tex = device.makeTexture(descriptor: texDesc)!
      let rpDesc = MTLRenderPassDescriptor()
      rpDesc.colorAttachments[0].texture = tex

      let buf = device.makeBuffer(length: 4, options: MTLResourceOptions.storageModeShared)!
      let icbDesc = MTLIndirectCommandBufferDescriptor()
      icbDesc.commandTypes = [.draw, .drawIndexed]
      icbDesc.inheritBuffers = false
      icbDesc.maxVertexBufferBindCount = 1
      icbDesc.maxFragmentBufferBindCount = 1
      let icb = device.makeIndirectCommandBuffer(descriptor: icbDesc, maxCommandCount: 4, options: MTLResourceOptions.storageModeShared )!

      /* Call APIs */

      let encoder = cmdBuf.makeRenderCommandEncoder(descriptor: rpDesc)!
      let cmd = icb.indirectRenderCommandAt(0)
      cmd.setVertexBuffer(buf, offset: 0, at: 0)
      cmd.setFragmentBuffer(buf, offset: 0, at: 0)
      cmd.drawPrimitives(MTLPrimitiveType.triangle, vertexStart: 0, vertexCount: 0, instanceCount: 0, baseInstance: 0)
      let cmd2 = icb.indirectRenderCommandAt(1)
      cmd2.drawIndexedPrimitives(MTLPrimitiveType.triangle, indexCount: 0, indexType: MTLIndexType.uint16, indexBuffer: buf, indexBufferOffset: 0, instanceCount: 0, baseVertex: 0, baseInstance: 0)
      let cmd3 = icb.indirectRenderCommandAt(2)
      cmd3.reset()
      icb.reset(0..<5)
      encoder.executeCommandsInBuffer(icb, range:0..<5)
      encoder.endEncoding()
    }

  }
}

  MetalTests.test("MTLBuffer") {
    func apiAvailabilityTest() {

      /* Setup */

      let device = MTLCreateSystemDefaultDevice()!
      #if os(macOS)
        let options = MTLResourceOptions.storageModeManaged
      #else
        let options = MTLResourceOptions.storageModePrivate
      #endif
      let buf = device.makeBuffer(length: 4, options: options)!

      /* Call APIs */

      #if os(macOS)
        buf.didModifyRange(0..<4)
      #endif
      buf.addDebugMarker("test marker", range: 0..<4)
    }
  }

  MetalTests.test("MTLComputeCommandEncoder") {
    func apiAvailabilityTest(heapDesc: MTLHeapDescriptor) {

      /* Setup */

      let device = MTLCreateSystemDefaultDevice()!
      let queue = device.makeCommandQueue()!
      let cmdBuf = queue.makeCommandBuffer()!

      #if os(macOS)
        let options = MTLResourceOptions.storageModeManaged
      #else
        let options = MTLResourceOptions.storageModePrivate
      #endif
      let buf = device.makeBuffer(length: 4, options: options)!
      let tex = device.makeTexture(descriptor: MTLTextureDescriptor())!
      heapDesc.size = 4
      let heap = device.makeHeap(descriptor: heapDesc)!
      let smplr = device.makeSamplerState(descriptor: MTLSamplerDescriptor())

      /* Call APIs */

      let encoder = cmdBuf.makeComputeCommandEncoder()!
      encoder.useResources([buf], usage: MTLResourceUsage.read)
      encoder.useHeaps([heap])
      encoder.setBuffers([buf], offsets: [0], range: 0..<1)
      encoder.setTextures([tex], range: 0..<1)
      encoder.setSamplerStates([smplr], range: 0..<1)
      encoder.setSamplerStates(
        [smplr], lodMinClamps: [0], lodMaxClamps: [0], range: 0..<1)
        if #available(macOS 10.14, iOS 12.0, tvOS 12.0, *)
        {
            encoder.memoryBarrier(resources: [buf])
            /* encoder.memoryBarrier(scope: MTLBarrierScope.buffers) */
        }
      encoder.endEncoding()
    }
  }

  MetalTests.test("MTLDevice") {
    func apiAvailabilityTest() {

      /* Setup */

      let device = MTLCreateSystemDefaultDevice()!

      /* Call APIs */

      var samplePositions : [MTLSamplePosition]
      if (device.supportsTextureSampleCount(2)) {
        samplePositions = device.getDefaultSamplePositions(sampleCount: 2)
      }
      else if (device.supportsTextureSampleCount(4)) {
        samplePositions = device.getDefaultSamplePositions(sampleCount: 4)
      }
      else {
        expectUnreachable("device unexpectedly does not support sample count 2 or 4")
      }
    }
  }

  MetalTests.test("MTLFunctionConstantValues") {
    func apiAvailabilityTest() {

      /* Call APIs */

      let vals = MTLFunctionConstantValues()
      vals.setConstantValues([0], type: MTLDataType.float, range: 0..<1)
    }
  }

  MetalTests.test("MTLRenderCommandEncoder") {
    func apiAvailabilityTest(heapDesc: MTLHeapDescriptor) {

      /* Setup */
      let device = MTLCreateSystemDefaultDevice()!
      let queue = device.makeCommandQueue()!
      let cmdBuf = queue.makeCommandBuffer()!

      #if os(macOS)
        let options = MTLResourceOptions.storageModeManaged
      #else
        let options = MTLResourceOptions.storageModePrivate
      #endif
      let buf = device.makeBuffer(length: 4, options: options)!
      let texDesc = MTLTextureDescriptor()
      texDesc.usage = MTLTextureUsage.renderTarget
      let tex = device.makeTexture(descriptor: texDesc)!
      heapDesc.size = 4
      let heap = device.makeHeap(descriptor: heapDesc)!
      let smplr = device.makeSamplerState(descriptor: MTLSamplerDescriptor())
      let rpDesc = MTLRenderPassDescriptor()
      rpDesc.colorAttachments[0].texture = tex

      /* Call APIs */

      let encoder = cmdBuf.makeRenderCommandEncoder(descriptor: rpDesc)!
      encoder.useResources([buf], usage: MTLResourceUsage.read)
      encoder.useHeaps([heap])
      #if os(macOS)
        encoder.setViewports([MTLViewport()])
        encoder.setScissorRects([MTLScissorRect(x:0, y:0, width:1, height:1)])
      #endif
      encoder.setVertexBuffers([buf], offsets: [0], range: 0..<1)
      encoder.setVertexTextures([tex], range: 0..<1)
      encoder.setVertexSamplerStates([smplr], range: 0..<1)
      encoder.setVertexSamplerStates(
        [smplr], lodMinClamps: [0], lodMaxClamps: [0], range: 0..<1)
      encoder.setFragmentBuffers([buf], offsets: [0], range: 0..<1)
      encoder.setFragmentTextures([tex], range: 0..<1)
      encoder.setFragmentSamplerStates([smplr], range: 0..<1)
      encoder.setFragmentSamplerStates(
        [smplr], lodMinClamps: [0], lodMaxClamps: [0], range: 0..<1)
     #if os(iOS)
         encoder.setTileBuffers([buf], offsets: [0], range: 0..<1)
         encoder.setTileTextures([tex], range: 0..<1)
         encoder.setTileSamplerStates([smplr], range: 0..<1)
         encoder.setTileSamplerStates(
           [smplr], lodMinClamps: [0], lodMaxClamps: [0], range: 0..<1)
     #endif
      #if os(OSX)
        if #available(macOS 10.14, *)
        {
            encoder.memoryBarrier(resources: [buf], after:MTLRenderStages.fragment, before:MTLRenderStages.vertex)
            /* encoder.memoryBarrier(scope: MTLBarrierScope.renderTargets, after:MTLRenderStages.fragment, before:MTLRenderStages.vertex) */
        }
      #endif
      encoder.endEncoding()
    }
  }

  MetalTests.test("MTLRenderPassDescriptor") {
    func apiAvailabilityTest() {

      /* Setup */

      let rpDesc = MTLRenderPassDescriptor()

      /* Call APIs */

      rpDesc.setSamplePositions(
        [MTLSamplePosition(x:0.25,y:0.75), MTLSamplePosition(x:0.75, y:0.25)])
      _ = rpDesc.getSamplePositions()
    }
  }

  MetalTests.test("MTLTexture") {
    func apiAvailabilityTest() {
      /* Setup */

      let device = MTLCreateSystemDefaultDevice()!
      let texDesc = MTLTextureDescriptor()
      texDesc.usage = MTLTextureUsage.renderTarget
      let tex = device.makeTexture(descriptor: texDesc)!

      /* Call APIs */

      let _ = tex.makeTextureView(
        pixelFormat: texDesc.pixelFormat,
        textureType: texDesc.textureType,
        levels: 0..<1,
        slices: 0..<1)
    }
  }
}

runAllTests()
