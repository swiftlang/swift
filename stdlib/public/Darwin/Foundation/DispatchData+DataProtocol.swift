//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//


import Dispatch

extension DispatchData : DataProtocol {
    public struct Region : DataProtocol, ContiguousBytes {
        internal let bytes: UnsafeBufferPointer<UInt8>
        internal let index: DispatchData.Index
        internal let owner: DispatchData
        internal init(bytes: UnsafeBufferPointer<UInt8>, index: DispatchData.Index, owner: DispatchData) {
            self.bytes = bytes
            self.index = index
            self.owner = owner
        }

        public var regions: CollectionOfOne<Region> {
            return CollectionOfOne(self)
        }

        public subscript(position: DispatchData.Index) -> UInt8 {
            precondition(index <= position && position <= index + bytes.count)
            return bytes[position - index]
        }

        public var startIndex: DispatchData.Index {
            return index
        }

        public var endIndex: DispatchData.Index {
            return index + bytes.count
        }

        public func withUnsafeBytes<ResultType>(_ body: (UnsafeRawBufferPointer) throws -> ResultType) rethrows -> ResultType {
            return try body(UnsafeRawBufferPointer(bytes))
        }
    }

    public var regions: [Region] {
        var regions = [Region]()
        enumerateBytes { (bytes, index, stop) in
            regions.append(Region(bytes: bytes, index: index, owner: self))
        }
        return regions
    }
}
