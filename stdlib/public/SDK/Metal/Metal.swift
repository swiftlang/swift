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

@available(swift 4)
@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLBlitCommandEncoder {
    
    public func fill(buffer: MTLBuffer, range: Range<Int>, value: UInt8) {
        fill(buffer: buffer, range: NSMakeRange(range.lowerBound, range.count), value: value)
    }
}

@available(swift 4)
@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLBuffer {
    
#if os(OSX)
    @available(macOS, introduced: 10.11)
    public func didModifyRange(_ range: Range<Int>) {
        didModifyRange(NSMakeRange(range.lowerBound, range.count))
    }
#endif
    
    @available(macOS 10.12, iOS 10.0, tvOS 10.0, *)
    public func addDebugMarker(_ marker: String, range: Range<Int>) {
        addDebugMarker(marker, range: NSMakeRange(range.lowerBound, range.count))
    }
}

@available(swift 4)
@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLComputeCommandEncoder {
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func useResources(_ resources: [MTLResource], usage: MTLResourceUsage) {
        useResources(resources, count: resources.count, usage: usage)
    }
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func useHeaps(_ heaps: [MTLHeap]) {
        useHeaps(heaps, count: heaps.count)
    }
    
    public func setBuffers(_ buffers: [MTLBuffer?], offsets: [Int], range: Range<Int>) {
        setBuffers(buffers, offsets: offsets, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    public func setTextures(_ textures: [MTLTexture?], range: Range<Int>) {
        setTextures(textures, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    public func setSamplerStates(_ samplers: [MTLSamplerState?], range: Range<Int>) {
        setSamplerStates(samplers, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    public func setSamplerStates(_ samplers: [MTLSamplerState?], lodMinClamps: [Float], lodMaxClamps: [Float], range: Range<Int>) {
        setSamplerStates(samplers, lodMinClamps: lodMinClamps, lodMaxClamps: lodMaxClamps, with: NSMakeRange(range.lowerBound, range.count))
    }
}

@available(swift 4)
@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLDevice {
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func getDefaultSamplePositions(sampleCount: Int) -> [MTLSamplePosition] {
        var positions = [MTLSamplePosition](repeating: MTLSamplePosition(x: 0,y: 0), count: sampleCount)
        getDefaultSamplePositions(&positions, count: sampleCount)
        return positions
    }
}

#if os(OSX)
@available(swift 4)
@available(macOS 10.13, *)
public func MTLCopyAllDevicesWithObserver(handler: @escaping MTLDeviceNotificationHandler) -> (devices:[MTLDevice], observer:NSObject) {
    var resultTuple: (devices:[MTLDevice], observer:NSObject)
    resultTuple.observer = NSObject()
    resultTuple.devices = MTLCopyAllDevicesWithObserver(AutoreleasingUnsafeMutablePointer<NSObjectProtocol?>(&resultTuple.observer), handler)
    return resultTuple
}
#endif

@available(swift 4)
@available(macOS 10.12, iOS 10.0, tvOS 10.0, *)
extension MTLFunctionConstantValues {
    
    public func setConstantValues(_ values: UnsafeRawPointer, type: MTLDataType, range: Range<Int>) {
        setConstantValues(values, type: type, with: NSMakeRange(range.lowerBound, range.count))
    }
    
}

@available(swift 4)
@available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
extension MTLArgumentEncoder {
    
    public func setBuffers(_ buffers: [MTLBuffer?], offsets: [Int], range: Range<Int>) {
        setBuffers(buffers, offsets: offsets, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    public func setTextures(_ textures: [MTLTexture?], range: Range<Int>) {
        setTextures(textures, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    public func setSamplerStates(_ samplers: [MTLSamplerState?], range: Range<Int>) {
        setSamplerStates(samplers, with: NSMakeRange(range.lowerBound, range.count))
    }
}

@available(swift 4)
@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLRenderCommandEncoder {
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func useResources(_ resources: [MTLResource], usage: MTLResourceUsage) {
        useResources(resources, count: resources.count, usage: usage)
    }
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func useHeaps(_ heaps: [MTLHeap]) {
        useHeaps(heaps, count: heaps.count)
    }
    
#if os(OSX)
    @available(macOS 10.13, *)
    public func setViewports(_ viewports: [MTLViewport]) {
        setViewports(viewports, count: viewports.count)
    }
    
    @available(macOS 10.13, *)
    public func setScissorRects(_ scissorRects: [MTLScissorRect]) {
        setScissorRects(scissorRects, count: scissorRects.count)
    }
#endif
    
    public func setVertexBuffers(_ buffers: [MTLBuffer?], offsets: [Int], range: Range<Int>) {
        setVertexBuffers(buffers, offsets: offsets, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    public func setVertexTextures(_ textures: [MTLTexture?], range: Range<Int>) {
        setVertexTextures(textures, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    public func setVertexSamplerStates(_ samplers: [MTLSamplerState?], range: Range<Int>) {
        setVertexSamplerStates(samplers, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    public func setVertexSamplerStates(_ samplers: [MTLSamplerState?], lodMinClamps: [Float], lodMaxClamps: [Float], range: Range<Int>) {
        setVertexSamplerStates(samplers, lodMinClamps: lodMinClamps, lodMaxClamps: lodMaxClamps, with: NSMakeRange(range.lowerBound, range.count))
    }

    public func setFragmentBuffers(_ buffers: [MTLBuffer?], offsets: [Int], range: Range<Int>) {
        setFragmentBuffers(buffers, offsets: offsets, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    public func setFragmentTextures(_ textures: [MTLTexture?], range: Range<Int>) {
        setFragmentTextures(textures, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    public func setFragmentSamplerStates(_ samplers: [MTLSamplerState?], range: Range<Int>) {
        setFragmentSamplerStates(samplers, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    public func setFragmentSamplerStates(_ samplers: [MTLSamplerState?], lodMinClamps: [Float], lodMaxClamps: [Float], range: Range<Int>) {
        setFragmentSamplerStates(samplers, lodMinClamps: lodMinClamps, lodMaxClamps: lodMaxClamps, with: NSMakeRange(range.lowerBound, range.count))
    }

#if os(iOS)

    @available(iOS 11.0, *)
    public func setTileBuffers(_ buffers: [MTLBuffer?], offsets: [Int], range: Range<Int>) {
        __setTileBuffers(buffers, offsets: offsets, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    @available(iOS 11.0, *)
    public func setTileTextures(_ textures: [MTLTexture?], range: Range<Int>) {
        __setTileTextures(textures, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    @available(iOS 11.0, *)
    public func setTileSamplerStates(_ samplers: [MTLSamplerState?], range: Range<Int>) {
        __setTileSamplerStates(samplers, with: NSMakeRange(range.lowerBound, range.count))
    }
    
    @available(iOS 11.0, *)
    public func setTileSamplerStates(_ samplers: [MTLSamplerState?], lodMinClamps: [Float], lodMaxClamps: [Float], range: Range<Int>) {
        __setTileSamplerStates(samplers, lodMinClamps: lodMinClamps, lodMaxClamps: lodMaxClamps, with: NSMakeRange(range.lowerBound, range.count))
    }
#endif
}

@available(swift 4)
@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLRenderPassDescriptor {
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func setSamplePositions(_ positions: [MTLSamplePosition]) {
        setSamplePositions(positions, count: positions.count)
    }
    
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, *)
    public func getSamplePositions() -> [MTLSamplePosition] {
        let numPositions = getSamplePositions(nil, count: 0)
        var positions = [MTLSamplePosition](repeating: MTLSamplePosition(x: 0,y: 0), count: numPositions)
        getSamplePositions(&positions, count: numPositions)
        return positions
    }
    
}

@available(swift 4)
@available(macOS 10.11, iOS 8.0, tvOS 8.0, *)
extension MTLTexture {
    
    @available(macOS 10.11, iOS 9.0, tvOS 9.0, *)
    public func makeTextureView(pixelFormat: MTLPixelFormat, textureType: MTLTextureType, levels levelRange: Range<Int>, slices sliceRange: Range<Int>) -> MTLTexture? {
        return makeTextureView(pixelFormat: pixelFormat, textureType: textureType, levels: NSMakeRange(levelRange.lowerBound, levelRange.count), slices: NSMakeRange(sliceRange.lowerBound, sliceRange.count))
    }
}
