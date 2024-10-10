import Swift

public enum PointerParam {
    case countedBy(pointer: Int, count: String)
    case sizedBy(pointer: Int, size: String)
    case endedBy(start: Int, end: Int)
    case nonescaping(pointer: Int)
}

@attached(peer, names: overloaded)
public macro PointerBounds(_ paramInfo: PointerParam...) =
    #externalMacro(module: "SwiftMacros", type: "PointerBoundsMacro")

// Stub interfaces for testing until Span lands
public struct Span<T> {
    public var count: Int
    var ptr: UnsafeBufferPointer<T>
    public func withUnsafeBufferPointer<R>(_ body: (UnsafeBufferPointer<T>) throws -> R) rethrows -> R {
        return try body(ptr)
    }
}
public protocol RawSpan {
    var byteCount: Int { get }
    func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R
}
public struct MutableSpan<T> {
    public var count: Int
    var ptr: UnsafeMutableBufferPointer<T>
    public func withUnsafeBufferPointer<R>(_ body: (UnsafeMutableBufferPointer<T>) throws -> R) rethrows -> R {
        return try body(ptr)
    }
}
public protocol MutableRawSpan {
    var byteCount: Int { get }
    func withUnsafeBytes<R>(_ body: (UnsafeMutableRawBufferPointer) throws -> R) rethrows -> R
}
