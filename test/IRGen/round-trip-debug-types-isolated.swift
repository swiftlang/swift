// RUN: %target-build-swift -g %s -emit-ir | %FileCheck %s

// Check that the IRGenDebugInfo type reconstruction
// (round-trip-debug-types) doesn't crash.
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$sSWxs5Error_pIgyrzo_D"

class __DataStorage {
    public init(bytes: UnsafeRawPointer, count: Int) {
        _representation = _Representation(UnsafeRawBufferPointer(start: bytes, count: count))
    }

    struct InlineData {
        typealias Buffer = (UInt8, UInt8, UInt8)
        var bytes: Buffer
        var length: UInt8

        init(count: Int = 0) {
            bytes = (UInt8(0), UInt8(0), UInt8(0))
            length = UInt8(count)
        }
        
        func withUnsafeBytes<Result>(_ apply: (UnsafeRawBufferPointer) throws -> Result) rethrows -> Result {
            let count = Int(length)
            return try Swift.withUnsafeBytes(of: bytes) { (rawBuffer) throws -> Result in
                return try apply(UnsafeRawBufferPointer(start: rawBuffer.baseAddress, count: count))
            }
        }
    }

    enum _Representation {
        case empty
        case inline(InlineData)

        init(_ buffer: UnsafeRawBufferPointer) {
            self = .empty
        }

        func withUnsafeBytes<Result>(_ apply: (UnsafeRawBufferPointer) throws -> Result) rethrows -> Result {
            switch self {
            case .empty:
                let empty = InlineData()
                return try empty.withUnsafeBytes(apply)
            case .inline(let inline):
                return try inline.withUnsafeBytes(apply)
            }
        }
    }

    var _representation: _Representation
}
    
