// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %target-swift-ide-test -print-module -module-to-print=CommentsClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --strict-whitespace --match-full-lines

// Check that doc comments are carried over from clang to the safe macro expansion.

// CHECK:func begin()
// CHECK-NEXT:func lineComment(_ len: {{.*}}, _ p: UnsafeMutablePointer<{{.*}}>!)
// CHECK-NEXT:/// line doc comment
// CHECK-NEXT:/// 
// CHECK-NEXT:/// Here's a more complete description.
// CHECK-NEXT:///
// CHECK-NEXT:/// @param len the buffer length
// CHECK-NEXT:/// @param p the buffer
// CHECK-NEXT:func lineDocComment(_ len: {{.*}}, _ p: UnsafeMutablePointer<{{.*}}>!)
// CHECK-NEXT:func blockComment(_ len: {{.*}}, _ p: UnsafeMutablePointer<{{.*}}>!)
// CHECK-NEXT:/**
// CHECK-NEXT: * block doc comment
// CHECK-NEXT: * 
// CHECK-NEXT: * NB: it's very important to pass the correct length to this function
// CHECK-NEXT: * @param len don't mess this one up
// CHECK-NEXT: * @param p   some integers to play with
// CHECK-NEXT: */
// CHECK-NEXT:func blockDocComment(_ len: {{.*}}, _ p: UnsafeMutablePointer<{{.*}}>!)
// CHECK-NEXT:@_alwaysEmitIntoClient public func blockComment(_ p: UnsafeMutableBufferPointer<{{.*}}>)
// CHECK-NEXT:/**
// CHECK-NEXT: * block doc comment
// CHECK-NEXT: * 
// CHECK-NEXT: * NB: it's very important to pass the correct length to this function
// CHECK-NEXT: * @param len don't mess this one up
// CHECK-NEXT: * @param p   some integers to play with
// CHECK-NEXT: */
// CHECK-NEXT:@_alwaysEmitIntoClient public func blockDocComment(_ p: UnsafeMutableBufferPointer<{{.*}}>)
// CHECK-NEXT:@_alwaysEmitIntoClient public func lineComment(_ p: UnsafeMutableBufferPointer<{{.*}}>)
// CHECK-NEXT:/// line doc comment
// CHECK-NEXT:/// 
// CHECK-NEXT:/// Here's a more complete description.
// CHECK-NEXT:///
// CHECK-NEXT:/// @param len the buffer length
// CHECK-NEXT:/// @param p the buffer
// CHECK-NEXT:@_alwaysEmitIntoClient public func lineDocComment(_ p: UnsafeMutableBufferPointer<{{.*}}>)