//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Metal // Clang module

@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLBlitCommandEncoder {
    
    public func fill(buffer: MTLBuffer, range: Range<Int>, value: UInt8) {
        __fill(buffer, range: NSRange(location: range.lowerBound, length: range.count), value: value)
    }

    @available(macOS 10.14, iOS 12.0, tvOS 12.0, *)
    public func resetCommandsInBuffer(_ buffer: MTLIndirectCommandBuffer, range: Range<Int>) {
        __resetCommands(in: buffer, with: NSRange(location: range.lowerBound, length: range.count))
    }

    @available(macOS 10.14, iOS 12.0, tvOS 12.0, *)
    public func copyIndirectCommandBuffer (_ buffer: MTLIndirectCommandBuffer, sourceRange: Range<Int>, destination: MTLIndirectCommandBuffer, destinationIndex: Int) {
        __copy (buffer, sourceRange: NSRange(location: sourceRange.lowerBound, length: sourceRange.count),destination: destination, destinationIndex: destinationIndex)
    }
    @available(macOS 10.14, iOS 12.0, tvOS 12.0, *)
    public func optimizeIndirectCommandBuffer (_ buffer: MTLIndirectCommandBuffer, range: Range<Int>) {
        __optimizeIndirectCommandBuffer(buffer, with: NSRange(location: range.lowerBound, length: range.count))
    }
}

@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLBuffer {
    
#if os(macOS)
    @available(macOS, introduced: 10.11)
    public func didModifyRange(_ range: Range<Int>) {
        __didModifyRange(NSRange(location: range.lowerBound, length: range.count))
    }
#endif
    
    @available(macOS 10.12, iOS 10.0, tvOS 10.0, *)
    public func addDebugMarker(_ marker: String, range: Range<Int>) {
        __addDebugMarker(marker, range: NSRange(location: range.lowerBound, length: range.count))
    }
}

@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLComputeCommandEncoder {
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func useResources(_ resources: [MTLResource], usage: MTLResourceUsage) {
        __use(resources, count: resources.count, usage: usage)
    }
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func useHeaps(_ heaps: [MTLHeap]) {
        __use(heaps, count: heaps.count)
    }
    
    public func setBuffers(_ buffers: [MTLBuffer?], offsets: [Int], range: Range<Int>) {
        __setBuffers(buffers, offsets: offsets, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    public func setTextures(_ textures: [MTLTexture?], range: Range<Int>) {
        __setTextures(textures, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    public func setSamplerStates(_ samplers: [MTLSamplerState?], range: Range<Int>) {
        __setSamplerStates(samplers, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    public func setSamplerStates(_ samplers: [MTLSamplerState?], lodMinClamps: [Float], lodMaxClamps: [Float], range: Range<Int>) {
        __setSamplerStates(samplers, lodMinClamps: lodMinClamps, lodMaxClamps: lodMaxClamps, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    @available(macOS 10.14, iOS 12.0, tvOS 12.0, *)
    public func memoryBarrier(resources:[MTLResource]) {
        __memoryBarrier(resources: resources, count: resources.count)
    }
}

@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLDevice {
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func getDefaultSamplePositions(sampleCount: Int) -> [MTLSamplePosition] {
        var positions = [MTLSamplePosition](repeating: MTLSamplePosition(x: 0,y: 0), count: sampleCount)
        __getDefaultSamplePositions(&positions, count: sampleCount)
        return positions
    }
}

#if os(macOS)
@available(swift 4)
@available(macOS 10.13, *)
public func MTLCopyAllDevicesWithObserver(handler: @escaping MTLDeviceNotificationHandler) -> (devices:[MTLDevice], observer:NSObject) {
    var observer: NSObjectProtocol?
    let devices = __MTLCopyAllDevicesWithObserver(&observer, handler)
    // FIXME: The force cast here isn't great – ideally we would return the
    // observer as an NSObjectProtocol.
    return (devices, observer as! NSObject)
}
#endif

@available(macOS 10.12, iOS 10.0, tvOS 10.0, *)
extension MTLFunctionConstantValues {
    
    public func setConstantValues(_ values: UnsafeRawPointer, type: MTLDataType, range: Range<Int>) {
        __setConstantValues(values, type: type, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
}

@available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
extension MTLArgumentEncoder {
    
    public func setBuffers(_ buffers: [MTLBuffer?], offsets: [Int], range: Range<Int>) {
        __setBuffers(buffers, offsets: offsets, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    public func setTextures(_ textures: [MTLTexture?], range: Range<Int>) {
        __setTextures(textures, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    public func setSamplerStates(_ samplers: [MTLSamplerState?], range: Range<Int>) {
        __setSamplerStates(samplers, with: NSRange(location: range.lowerBound, length: range.count))
    }

    #if os(macOS)
    @available(macOS 10.14, *)
    public func setRenderPipelineStates(_ pipelines: [MTLRenderPipelineState?], range: Range<Int>) {
        __setRenderPipelineStates(pipelines, with: NSRange(location: range.lowerBound, length: range.count))
    }
    #endif
    
    @available(macOS 10.14, iOS 12.0, tvOS 12.0, *)
    public func setIndirectCommandBuffers(_ buffers: [MTLIndirectCommandBuffer?], range: Range<Int>) {
        __setIndirectCommandBuffers(buffers, with: NSRange(location: range.lowerBound, length: range.count))
    }
}

@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLRenderCommandEncoder {
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func useResources(_ resources: [MTLResource], usage: MTLResourceUsage) {
        __use(resources, count: resources.count, usage: usage)
    }
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func useHeaps(_ heaps: [MTLHeap]) {
        __use(heaps, count: heaps.count)
    }

#if os(macOS) || os(iOS)
    @available(macOS 10.13, iOS 12.0, *)
    public func setViewports(_ viewports: [MTLViewport]) {
        __setViewports(viewports, count: viewports.count)
    }
    
    @available(macOS 10.13, iOS 12.0, *)
    public func setScissorRects(_ scissorRects: [MTLScissorRect]) {
        __setScissorRects(scissorRects, count: scissorRects.count)
    }
#endif
    
    public func setVertexBuffers(_ buffers: [MTLBuffer?], offsets: [Int], range: Range<Int>) {
        __setVertexBuffers(buffers, offsets: offsets, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    public func setVertexTextures(_ textures: [MTLTexture?], range: Range<Int>) {
        __setVertexTextures(textures, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    public func setVertexSamplerStates(_ samplers: [MTLSamplerState?], range: Range<Int>) {
        __setVertexSamplerStates(samplers, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    public func setVertexSamplerStates(_ samplers: [MTLSamplerState?], lodMinClamps: [Float], lodMaxClamps: [Float], range: Range<Int>) {
        __setVertexSamplerStates(samplers, lodMinClamps: lodMinClamps, lodMaxClamps: lodMaxClamps, with: NSRange(location: range.lowerBound, length: range.count))
    }

    public func setFragmentBuffers(_ buffers: [MTLBuffer?], offsets: [Int], range: Range<Int>) {
        __setFragmentBuffers(buffers, offsets: offsets, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    public func setFragmentTextures(_ textures: [MTLTexture?], range: Range<Int>) {
        __setFragmentTextures(textures, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    public func setFragmentSamplerStates(_ samplers: [MTLSamplerState?], range: Range<Int>) {
        __setFragmentSamplerStates(samplers, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    public func setFragmentSamplerStates(_ samplers: [MTLSamplerState?], lodMinClamps: [Float], lodMaxClamps: [Float], range: Range<Int>) {
        __setFragmentSamplerStates(samplers, lodMinClamps: lodMinClamps, lodMaxClamps: lodMaxClamps, with: NSRange(location: range.lowerBound, length: range.count))
    }

#if os(iOS)

    @available(iOS 11.0, *)
    public func setTileBuffers(_ buffers: [MTLBuffer?], offsets: [Int], range: Range<Int>) {
        __setTileBuffers(buffers, offsets: offsets, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    @available(iOS 11.0, *)
    public func setTileTextures(_ textures: [MTLTexture?], range: Range<Int>) {
        __setTileTextures(textures, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    @available(iOS 11.0, *)
    public func setTileSamplerStates(_ samplers: [MTLSamplerState?], range: Range<Int>) {
        __setTileSamplerStates(samplers, with: NSRange(location: range.lowerBound, length: range.count))
    }
    
    @available(iOS 11.0, *)
    public func setTileSamplerStates(_ samplers: [MTLSamplerState?], lodMinClamps: [Float], lodMaxClamps: [Float], range: Range<Int>) {
        __setTileSamplerStates(samplers, lodMinClamps: lodMinClamps, lodMaxClamps: lodMaxClamps, with: NSRange(location: range.lowerBound, length: range.count))
    }
#endif
    
#if os(macOS)
    @available(macOS 10.14, *)
    public func memoryBarrier(resources: [MTLResource], after: MTLRenderStages, before: MTLRenderStages) {
        __memoryBarrier(resources: resources, count: resources.count, after: after, before: before)
    }
#endif

    @available(macOS 10.14, iOS 12.0, tvOS 12.0, *)
    public func executeCommandsInBuffer(_ buffer: MTLIndirectCommandBuffer, range: Range<Int>) {
        __executeCommands(in: buffer, with: NSRange(location: range.lowerBound, length: range.count))
    }

    #if os(macOS)
    @available(macOS 10.14, *)
    public func executeCommandsInBuffer(_ buffer: MTLIndirectCommandBuffer, indirectBuffer indirectRangeBuffer: MTLBuffer, offset: Int) {
        __executeCommands(in: buffer, indirectBuffer: indirectRangeBuffer, indirectBufferOffset: offset)
    }
    #endif
}

@available(macOS 10.14, iOS 12.0, tvOS 12.0, *)
extension MTLIndirectCommandBuffer {
    public func reset(_ range: Range<Int>) {
        __reset(with: NSRange(location: range.lowerBound, length: range.count))
    }
}

@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLRenderPassDescriptor {
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func setSamplePositions(_ positions: [MTLSamplePosition]) {
        __setSamplePositions(positions, count: positions.count)
    }
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func getSamplePositions() -> [MTLSamplePosition] {
        let numPositions = __getSamplePositions(nil, count: 0)
        var positions = [MTLSamplePosition](repeating: MTLSamplePosition(x: 0,y: 0), count: numPositions)
        __getSamplePositions(&positions, count: numPositions)
        return positions
    }
    
}

@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLTexture {
    
    @available(macOS 10.11, iOS 9.0, tvOS 9.0, *)
    public func makeTextureView(pixelFormat: MTLPixelFormat, textureType: MTLTextureType, levels levelRange: Range<Int>, slices sliceRange: Range<Int>) -> MTLTexture? {
        return __newTextureView(with: pixelFormat, textureType: textureType, levels: NSRange(location: levelRange.lowerBound, length: levelRange.count), slices: NSRange(location: sliceRange.lowerBound, length: sliceRange.count))
    }
}
