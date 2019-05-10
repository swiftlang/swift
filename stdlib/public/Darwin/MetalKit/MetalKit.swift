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

@_exported import MetalKit // Clang module

@available(macOS 10.11, iOS 9.0, tvOS 9.0, *)
extension MTKMesh {
    public class func newMeshes(asset: MDLAsset, device: MTLDevice) throws -> (modelIOMeshes: [MDLMesh], metalKitMeshes: [MTKMesh]) {
        var modelIOMeshes: NSArray?
        let metalKitMeshes = try MTKMesh.__newMeshes(from: asset, device: device, sourceMeshes: &modelIOMeshes)
        return (modelIOMeshes: modelIOMeshes as! [MDLMesh], metalKitMeshes: metalKitMeshes)
    }
}

@available(swift 4)
@available(macOS 10.12, iOS 10.0, tvOS 10.0, *)
public func MTKModelIOVertexDescriptorFromMetalWithError(_ metalDescriptor: MTLVertexDescriptor) throws -> MDLVertexDescriptor {
    var error: NSError? = nil
    let result = __MTKModelIOVertexDescriptorFromMetalWithError(metalDescriptor, &error)
    if let error = error {
        throw error
    }
    return result
}

@available(swift 4)
@available(macOS 10.12, iOS 10.0, tvOS 10.0, *)
public func MTKMetalVertexDescriptorFromModelIOWithError(_ modelIODescriptor: MDLVertexDescriptor) throws -> MTLVertexDescriptor? {
    var error: NSError? = nil
    let result = __MTKMetalVertexDescriptorFromModelIOWithError(modelIODescriptor, &error)
    if let error = error {
        throw error
    }
    return result
}
