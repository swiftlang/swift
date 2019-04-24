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
public enum vImage {}

/// An enum that acts as a namespace for Swift overlays to vDSP based functions.
public enum vDSP {}

/// An enum that acts as a namespace for Swift overlays to vForce based functions.
public enum vForce {}

extension vDSP {
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public struct VectorizableFloat {
        public typealias Scalar = Float
    }
  
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public struct VectorizableDouble {
        public typealias Scalar = Double
    }
}
