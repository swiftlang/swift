// RUN: %target-swift-ide-test -print-module -module-to-print=CommentsClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x | %FileCheck %s --strict-whitespace --match-full-lines --check-prefixes=BOTH,IDE
// RUN: %target-swift-synthesize-interface -module-name CommentsClang -I %S/Inputs -o - | %FileCheck %s --strict-whitespace --match-full-lines --check-prefixes=BOTH,SYNTH

// Check that doc comments are carried over from clang to the safe macro expansion.

// IDE:func begin()
// SYNTH:public func begin()

// SYNTH-EMPTY:
// IDE-NEXT:func lineComment(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)
// SYNTH-NEXT:public func lineComment(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)

// SYNTH-EMPTY:
// BOTH-NEXT:/// line doc comment
// BOTH-NEXT:/// 
// BOTH-NEXT:/// Here's a more complete description.
// BOTH-NEXT:///
// BOTH-NEXT:/// @param len the buffer length
// BOTH-NEXT:/// @param p the buffer
// IDE-NEXT:func lineDocComment(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)
// SYNTH-NEXT:public func lineDocComment(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)

// SYNTH-EMPTY:
// IDE-NEXT:func blockComment(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)
// SYNTH-NEXT:public func blockComment(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)

// SYNTH-EMPTY:
// BOTH-NEXT:/**
// BOTH-NEXT: * block doc comment
// BOTH-NEXT: * 
// BOTH-NEXT: * NB: it's very important to pass the correct length to this function
// BOTH-NEXT: * @param len don't mess this one up
// BOTH-NEXT: * @param p   some integers to play with
// BOTH-NEXT: */
// IDE-NEXT:func blockDocComment(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)
// SYNTH-NEXT:public func blockDocComment(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)

// SYNTH-EMPTY:
// BOTH-NEXT:/// This is an auto-generated wrapper for safer interop
// IDE-NEXT:@_alwaysEmitIntoClient @_disfavoredOverload public func blockComment(_ p: UnsafeMutableBufferPointer<Int32>)
// SYNTH-NEXT:public func blockComment(_ p: UnsafeMutableBufferPointer<Int32>)

// SYNTH-EMPTY:
// BOTH-NEXT:/**
// BOTH-NEXT: * block doc comment
// BOTH-NEXT: * 
// BOTH-NEXT: * NB: it's very important to pass the correct length to this function
// BOTH-NEXT: * @param len don't mess this one up
// BOTH-NEXT: * @param p   some integers to play with
// BOTH-NEXT: */
// BOTH-NEXT:/// This is an auto-generated wrapper for safer interop
// IDE-NEXT:@_alwaysEmitIntoClient @_disfavoredOverload public func blockDocComment(_ p: UnsafeMutableBufferPointer<Int32>)
// SYNTH-NEXT:public func blockDocComment(_ p: UnsafeMutableBufferPointer<Int32>)

// SYNTH-EMPTY:
// BOTH-NEXT:/// This is an auto-generated wrapper for safer interop
// IDE-NEXT:@_alwaysEmitIntoClient @_disfavoredOverload public func lineComment(_ p: UnsafeMutableBufferPointer<Int32>)
// SYNTH-NEXT:public func lineComment(_ p: UnsafeMutableBufferPointer<Int32>)

// SYNTH-EMPTY:
// BOTH-NEXT:/// line doc comment
// BOTH-NEXT:/// 
// BOTH-NEXT:/// Here's a more complete description.
// BOTH-NEXT:///
// BOTH-NEXT:/// @param len the buffer length
// BOTH-NEXT:/// @param p the buffer
// BOTH-NEXT:/// This is an auto-generated wrapper for safer interop
// IDE-NEXT:@_alwaysEmitIntoClient @_disfavoredOverload public func lineDocComment(_ p: UnsafeMutableBufferPointer<Int32>)
// SYNTH-NEXT:public func lineDocComment(_ p: UnsafeMutableBufferPointer<Int32>)
