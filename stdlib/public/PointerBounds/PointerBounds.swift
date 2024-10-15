import Swift

/// Different ways to annotate pointer parameters using the `@PointerBounds` macro.
/// All indices into parameter lists start at 1. Indices __must__ be integer literals, and strings
/// __must__ be string literals, because their contents are parsed by the `@PointerBounds` macro.
/// Only 1 instance of `countedBy`, `sizedBy` or `endedBy` can refer to each pointer index, however
/// `nonescaping` is orthogonal to the rest and can (and should) overlap with other annotations.
public enum PointerParam {
    /// Corresponds to the C `__counted_by(count)` attribute.
    /// Parameter pointer: index of pointer in function parameter list. Must be of type
    /// `Unsafe[Mutable]Pointer<T>[?]`, i.e. not an `UnsafeRawPointer`.
    /// Parameter count: string containing valid Swift syntax containing the number of elements in
    /// the buffer.
    case countedBy(pointer: Int, count: String)
    /// Corresponds to the C `__sized_by(size)` attribute.
    /// Parameter pointer: index of pointer in function parameter list. Must be of type
    /// `Unsafe[Mutable]RawPointer[?]`, i.e. not an `UnsafePointer<T>`.
    /// Parameter count: string containing valid Swift syntax containing the size of the buffer,
    /// in bytes.
    case sizedBy(pointer: Int, size: String)
    /// Corresponds to the C `__ended_by(end)` attribute.
    /// Parameter start: index of pointer in function parameter list.
    /// Parameter end: index of pointer in function parameter list, pointing one past the end of
    /// the same buffer as `start`.
    case endedBy(start: Int, end: Int)
    /// Corresponds to the C `noescape` attribute. Allows generated wrapper to use `Span`-types
    /// instead of `UnsafeBuffer`-types, because it is known that the function doesn't capture the
    /// object past the lifetime of the function.
    /// Parameter pointer: index of pointer in function parameter list.
    case nonescaping(pointer: Int)
}

/// Generates a bounds safe wrapper for function with Unsafe[Mutable][Raw]Pointer[?] arguments.
/// Intended to be automatically attached to function declarations imported by ClangImporter.
/// The wrapper function will replace Unsafe[Mutable][Raw]Pointer[?] parameters with
/// [Mutable][Raw]Span[?] or Unsafe[Mutable][Raw]BufferPointer[?] if they have bounds information
/// attached. Where possible "count" parameters will be elided from the wrapper signature, instead
/// fetching the count from the buffer pointer. In these cases the bounds check is also skipped.
///
/// Currently not supported: return pointers, nested pointers, pointee "count" parameters, endedBy.
///
/// Parameter paramInfo: information about how the function uses the pointer passed to it. The
/// safety of the generated wrapper function depends on this info being extensive and accurate.
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
