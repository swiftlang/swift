// REQUIRES: swift_feature_SafeInteropWrappers

// Check that doc comments are carried over from clang to the safe macro expansion.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %generate-callers(module:CommentsClang) > %t/test.swift
// RUN: %verify-safe-wrappers %t/test.swift
// RUN: %dump-safe-wrappers %t/test.swift 2> %t/expansions.out
// RUN: diff %t/expansions.out %t/expansions.expected

//--- expansions.expected
@__swiftmacro_So11lineComment15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func lineComment(_ p: UnsafeMutableBufferPointer<Int32>) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe lineComment(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So14lineDocComment15_SwiftifyImportfMp_.swift
------------------------------
/// line doc comment
/// 
/// Here's a more complete description.
///
/// @param len the buffer length
/// @param p the buffer
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func lineDocComment(_ p: UnsafeMutableBufferPointer<Int32>) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe lineDocComment(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So12blockComment15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func blockComment(_ p: UnsafeMutableBufferPointer<Int32>) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe blockComment(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So15blockDocComment15_SwiftifyImportfMp_.swift
------------------------------
/**
 * block doc comment
 * 
 * NB: it's very important to pass the correct length to this function
 * @param len don't mess this one up
 * @param p   some integers to play with
 */
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func blockDocComment(_ p: UnsafeMutableBufferPointer<Int32>) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe blockDocComment(len, p.baseAddress!)
}
------------------------------
