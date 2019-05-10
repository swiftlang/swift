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

//===--- DataProtocol -----------------------------------------------------===//

extension Array: DataProtocol where Element == UInt8 {
    public var regions: CollectionOfOne<Array<UInt8>> {
        return CollectionOfOne(self)
    }
}

extension ArraySlice: DataProtocol where Element == UInt8 {
    public var regions: CollectionOfOne<ArraySlice<UInt8>> {
        return CollectionOfOne(self)
    }
}

extension ContiguousArray: DataProtocol where Element == UInt8 {
    public var regions: CollectionOfOne<ContiguousArray<UInt8>> {
        return CollectionOfOne(self)
    }
}

// FIXME: This currently crashes compilation in the Late Inliner.
// extension CollectionOfOne : DataProtocol where Element == UInt8 {
//     public typealias Regions = CollectionOfOne<Data>
//
//     public var regions: CollectionOfOne<Data> {
//         return CollectionOfOne<Data>(Data(self))
//     }
// }

extension EmptyCollection : DataProtocol where Element == UInt8 {
    public var regions: EmptyCollection<Data> {
        return EmptyCollection<Data>()
    }
}

extension Repeated: DataProtocol where Element == UInt8 {
    public typealias Regions = Repeated<Data>

    public var regions: Repeated<Data> {
        guard self.count > 0 else { return repeatElement(Data(), count: 0) }
        return repeatElement(Data(CollectionOfOne(self.first!)), count: self.count)
    }
}

//===--- MutableDataProtocol ----------------------------------------------===//

extension Array: MutableDataProtocol where Element == UInt8 { }

extension ContiguousArray: MutableDataProtocol where Element == UInt8 { }
