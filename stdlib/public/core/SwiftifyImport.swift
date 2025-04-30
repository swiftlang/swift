public enum _SwiftifyExpr {
    case param(_ index: Int)
    case `return`
    case `self`
}

public enum _DependenceType {
    case borrow
    case copy
}
/// Different ways to annotate pointer parameters using the `@_SwiftifyImport` macro.
/// All indices into parameter lists start at 1. Indices __must__ be integer literals, and strings
/// __must__ be string literals, because their contents are parsed by the `@_SwiftifyImport` macro.
/// Only 1 instance of `countedBy`, `sizedBy` or `endedBy` can refer to each pointer index, however
/// `nonescaping` is orthogonal to the rest and can (and should) overlap with other annotations.
///
/// This is not marked @available, because _SwiftifyImport is available for any target. Instances
/// of _SwiftifyInfo should ONLY be passed as arguments directly to _SwiftifyImport, so they should
/// not affect linkage since there are never any instances at runtime.
public enum _SwiftifyInfo {
    /// Corresponds to the C `__counted_by(count)` attribute.
    /// Parameter pointer: index of pointer in function parameter list. Must be of type
    /// `Unsafe[Mutable]Pointer<T>[?]`, i.e. not an `UnsafeRawPointer`.
    /// Parameter count: string containing valid Swift syntax containing the number of elements in
    /// the buffer.
    case countedBy(pointer: _SwiftifyExpr, count: String)
    /// Corresponds to the C `__sized_by(size)` attribute.
    /// Parameter pointer: index of pointer in function parameter list. Must be of type
    /// `Unsafe[Mutable]RawPointer[?]`, i.e. not an `UnsafePointer<T>`.
    /// Parameter count: string containing valid Swift syntax containing the size of the buffer,
    /// in bytes.
    case sizedBy(pointer: _SwiftifyExpr, size: String)
    /// Corresponds to the C `__ended_by(end)` attribute.
    /// Parameter start: index of pointer in function parameter list.
    /// Parameter end: index of pointer in function parameter list, pointing one past the end of
    /// the same buffer as `start`.
    case endedBy(start: _SwiftifyExpr, end: Int)
    /// Corresponds to the C `noescape` attribute. Allows generated wrapper to use `Span`-types
    /// instead of `UnsafeBuffer`-types, because it is known that the function doesn't capture the
    /// object past the lifetime of the function.
    /// Parameter pointer: index of pointer in function parameter list.
    case nonescaping(pointer: _SwiftifyExpr)
    /// Can express lifetime dependencies between inputs and outputs of a function.
    /// 'dependsOn' is the input on which the output 'pointer' depends.
    case lifetimeDependence(dependsOn: _SwiftifyExpr, pointer: _SwiftifyExpr, type: _DependenceType)
}

/// Generates a safe wrapper for function with Unsafe[Mutable][Raw]Pointer[?] or std::span arguments.
/// Intended to be automatically attached to function declarations imported by ClangImporter.
/// The wrapper function will replace Unsafe[Mutable][Raw]Pointer[?] parameters with
/// [Mutable][Raw]Span[?] or Unsafe[Mutable][Raw]BufferPointer[?] if they have bounds information
/// attached. Where possible "count" parameters will be elided from the wrapper signature, instead
/// fetching the count from the buffer pointer. In these cases the bounds check is also skipped.
/// It will replace some std::span arguments with Swift's Span type when sufficient information is
/// available.
///
/// Currently not supported: return pointers, nested pointers, pointee "count" parameters, endedBy.
///
/// Parameter paramInfo: information about how the function uses the pointer passed to it. The
/// safety of the generated wrapper function depends on this info being extensive and accurate.
#if hasFeature(Macros)
@attached(peer, names: overloaded)
public macro _SwiftifyImport(_ paramInfo: _SwiftifyInfo..., typeMappings: [String: String] = [:]) =
    #externalMacro(module: "SwiftMacros", type: "SwiftifyImportMacro")
#endif
