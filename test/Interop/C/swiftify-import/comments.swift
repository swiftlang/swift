// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %target-swift-ide-test -print-module -module-to-print=CommentsClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --strict-whitespace --match-full-lines

// Check that doc comments are carried over from clang to the safe macro expansion.

// CHECK:func begin()
// CHECK-NEXT:func lineComment(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)

// CHECK-NEXT:/// line doc comment
// CHECK-NEXT:/// 
// CHECK-NEXT:/// Here's a more complete description.
// CHECK-NEXT:///
// CHECK-NEXT:/// @param len the buffer length
// CHECK-NEXT:/// @param p the buffer
// CHECK-NEXT:func lineDocComment(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)

// CHECK-NEXT:func blockComment(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)

// CHECK-NEXT:/**
// CHECK-NEXT: * block doc comment
// CHECK-NEXT: * 
// CHECK-NEXT: * NB: it's very important to pass the correct length to this function
// CHECK-NEXT: * @param len don't mess this one up
// CHECK-NEXT: * @param p   some integers to play with
// CHECK-NEXT: */
// CHECK-NEXT:func blockDocComment(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)

// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @_disfavoredOverload public func blockComment(_ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT:/**
// CHECK-NEXT: * block doc comment
// CHECK-NEXT: * 
// CHECK-NEXT: * NB: it's very important to pass the correct length to this function
// CHECK-NEXT: * @param len don't mess this one up
// CHECK-NEXT: * @param p   some integers to play with
// CHECK-NEXT: */
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @_disfavoredOverload public func blockDocComment(_ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @_disfavoredOverload public func lineComment(_ p: UnsafeMutableBufferPointer<Int32>)

// CHECK-NEXT:/// line doc comment
// CHECK-NEXT:/// 
// CHECK-NEXT:/// Here's a more complete description.
// CHECK-NEXT:///
// CHECK-NEXT:/// @param len the buffer length
// CHECK-NEXT:/// @param p the buffer
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @_disfavoredOverload public func lineDocComment(_ p: UnsafeMutableBufferPointer<Int32>)
