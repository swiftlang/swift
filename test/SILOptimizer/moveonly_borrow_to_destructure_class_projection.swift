// rdar://133333278

// RUN: %target-swift-frontend -emit-sil -verify %s

public final class List<Element> where Element: ~Copyable {
    public private(set) var count: Int
            
    @inlinable
    public var capacity: Int {
        return buffer.count
    }
    
    @inlinable
    public static var defaultCapacity: Int { 2 }
    
    @_alwaysEmitIntoClient
    private var buffer: UnsafeMutableBufferPointer<Element>

    public init(emptyWithCapacity initialCapacity: Int = defaultCapacity) {
        precondition(initialCapacity >= 0)
        self.count = 0
        self.buffer = .allocate(capacity: initialCapacity)
    }
}

private struct ListProcessor: ~Copyable {
    fileprivate struct ListElement {
    }
    
    fileprivate var tasks: List<ListElement>
    fileprivate func process() {
        for _ in 0..<tasks.count {
        }
    }
}
