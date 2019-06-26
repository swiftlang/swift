//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// An enum that acts as a namespace for Swift overlays to vImage option sets and enums..
@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public enum vImage {}

/// An enum that acts as a namespace for Swift overlays to vDSP based functions.
@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public enum vDSP {}

/// An enum that acts as a namespace for Swift overlays to vForce based functions.
@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public enum vForce {}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public struct VectorizableFloat {
        public typealias Scalar = Float
    }
  
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public struct VectorizableDouble {
        public typealias Scalar = Double
    }
}
