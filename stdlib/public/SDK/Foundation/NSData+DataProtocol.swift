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


extension NSData : DataProtocol {

    @nonobjc
    public var startIndex: Int { return 0 }

    @nonobjc
    public var endIndex: Int { return length }

    @nonobjc
    public func lastRange<D, R>(of data: D, in r: R) -> Range<Int>? where D : DataProtocol, R : RangeExpression, NSData.Index == R.Bound {
        return Range<Int>(range(of: Data(data), options: .backwards, in: NSRange(r)))
    }

    @nonobjc
    public func firstRange<D, R>(of data: D, in r: R) -> Range<Int>? where D : DataProtocol, R : RangeExpression, NSData.Index == R.Bound {
        return Range<Int>(range(of: Data(data), in: NSRange(r)))
    }

    @nonobjc
    public var regions: [Data] {
        var datas = [Data]()
        enumerateBytes { (ptr, range, stop) in
            datas.append(Data(bytesNoCopy: UnsafeMutableRawPointer(mutating: ptr), count: range.length, deallocator: .custom({ (ptr: UnsafeMutableRawPointer, count: Int) -> Void in
                withExtendedLifetime(self) { }
            })))
        }
        return datas
    }

    @nonobjc
    public subscript(position: Int) -> UInt8 {
        var byte = UInt8(0)
        enumerateBytes { (ptr, range, stop) in
            if range.location <= position && position < range.upperBound {
                byte = ptr.load(fromByteOffset: range.location - position, as: UInt8.self)
                stop.pointee = true
            }
        }
        return byte
    }
}
