// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

import Foundation

public struct CustomCollection<T>: RandomAccessCollection {
    public typealias Indices = Range<Int>
    
    public var startIndex: Int { fatalError() }
    public var endIndex: Int { fatalError() }
    public var count: Int { fatalError() }
    
    public subscript(position: Int) -> T {
        get { fatalError() }
        set { fatalError() }
    }
    
    public var underestimatedCount: Int { fatalError() }
}

extension CustomCollection: ContiguousBytes where Element == UInt8 {
    public func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R {
        fatalError()
    }
}

extension CustomCollection: DataProtocol where Element == UInt8 {
    public var regions: CollectionOfOne<CustomCollection<UInt8>> {
        fatalError()
    }
}
