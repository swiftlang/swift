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

@_exported import ARKit

@available(iOS, introduced: 11.0)
extension ARCamera {
    /**
     A value describing the camera's tracking state.
     */
    @_frozen
    public enum TrackingState {
        public enum Reason {
            /** Tracking is limited due to initialization in progress. */
            case initializing
            
            /** Tracking is limited due to a excessive motion of the camera. */
            case excessiveMotion
            
            /** Tracking is limited due to a lack of features visible to the camera. */
            case insufficientFeatures

            /** Tracking is limited due to a relocalization in progress. */
            @available(iOS, introduced: 11.3)
            case relocalizing
        }
        
        /** Tracking is not available. */
        case notAvailable
        
        /** Tracking is limited. See tracking reason for details. */
        case limited(Reason)
        
        /** Tracking is normal. */
        case normal
    }
    
    /**
     The tracking state of the camera.
     */
    public var trackingState: TrackingState {
        switch __trackingState {
        case .notAvailable: return .notAvailable
        case .normal: return .normal
        case .limited:
            let reason: TrackingState.Reason
            
            if #available(iOS 11.3, *) {
                switch __trackingStateReason {
                case .initializing: reason = .initializing
                case .relocalizing: reason = .relocalizing
                case .excessiveMotion: reason = .excessiveMotion
                default: reason = .insufficientFeatures
                }
            }
            else {
                switch __trackingStateReason {
                case .initializing: reason = .initializing
                case .excessiveMotion: reason = .excessiveMotion
                default: reason = .insufficientFeatures
                }
            }
            
            return .limited(reason)
        }
    }
    
    @available(iOS, introduced: 12.0)
    @nonobjc
    public func unprojectPoint(
      _ point: CGPoint,
      ontoPlane planeTransform: simd_float4x4,
      orientation: UIInterfaceOrientation,
      viewportSize: CGSize
    ) -> simd_float3? {
        let result = __unprojectPoint(
          point,
          ontoPlaneWithTransform: planeTransform,
          orientation: orientation,
          viewportSize: viewportSize)
        if result.x.isNaN || result.y.isNaN || result.z.isNaN {
            return nil
        }
        
        return result
    }
}

@available(iOS, introduced: 12.0)
extension ARSCNView {
    @nonobjc public func unprojectPoint(
      _ point: CGPoint, ontoPlane planeTransform: simd_float4x4
    ) -> simd_float3? {
        let result = __unprojectPoint(
          point, ontoPlaneWithTransform: planeTransform)
        if result.x.isNaN || result.y.isNaN || result.z.isNaN {
            return nil
        }
        
        return result
    }
}

@available(iOS, introduced: 11.0)
extension ARPointCloud {
    /**
     The 3D points comprising the point cloud.
     */
    @nonobjc public var points: [vector_float3] {
        let buffer = UnsafeBufferPointer(start: __points, count: Int(__count))
        return Array(buffer)
    }
    
    /**
     The 3D point identifiers comprising the point cloud.
     */
    @nonobjc public var identifiers: [UInt64] {
        let buffer = UnsafeBufferPointer(start: __identifiers, count: Int(__count))
        return Array(buffer)
    }
}

@available(iOS, introduced: 11.0)
extension ARFaceGeometry {
    /**
     The mesh vertices of the geometry.
     */
    @nonobjc public var vertices: [vector_float3] {
        let buffer = UnsafeBufferPointer(start: __vertices, count: Int(__vertexCount))
        return Array(buffer)
    }

    /**
     The texture coordinates of the geometry.
     */
    @nonobjc public var textureCoordinates: [vector_float2] {
        let buffer = UnsafeBufferPointer(start: __textureCoordinates, count: Int(__textureCoordinateCount))
        return Array(buffer)
    }

    /**
     The triangle indices of the geometry.
     */
    @nonobjc public var triangleIndices: [Int16] {
        let buffer = UnsafeBufferPointer(start: __triangleIndices, count: Int(triangleCount * 3))
        return Array(buffer)
    }
}

@available(iOS, introduced: 11.3)
extension ARPlaneGeometry {
    /**
     The mesh vertices of the geometry.
     */
    @nonobjc public var vertices: [vector_float3] {
        let buffer = UnsafeBufferPointer(start: __vertices, count: Int(__vertexCount))
        return Array(buffer)
    }
    
    /**
     The texture coordinates of the geometry.
     */
    @nonobjc public var textureCoordinates: [vector_float2] {
        let buffer = UnsafeBufferPointer(start: __textureCoordinates, count: Int(__textureCoordinateCount))
        return Array(buffer)
    }
    
    /**
     The triangle indices of the geometry.
     */
    @nonobjc public var triangleIndices: [Int16] {
        let buffer = UnsafeBufferPointer(start: __triangleIndices, count: Int(triangleCount * 3))
        return Array(buffer)
    }
    
    /**
     The vertices of the geometry's outermost boundary.
     */
    @nonobjc public var boundaryVertices: [vector_float3] {
        let buffer = UnsafeBufferPointer(start: __boundaryVertices, count: Int(__boundaryVertexCount))
        return Array(buffer)
    }
}
