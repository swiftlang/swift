// RUN: not %target-swift-frontend -typecheck %s -I %S/Inputs -disable-availability-checking -diagnostic-style llvm 2>&1 | %FileCheck %s

import ForeignReferenceInvalid

// CHECK: error: cannot find retain function 'nonexistent' for reference type 'NonExistent'
// CHECK: error: cannot find release function 'nonexistent' for reference type 'NonExistent'
public func test(x: NonExistent) { }

// CHECK: error: reference type 'NoRetainRelease' must have 'retain:' Swift attribute
// CHECK: error: reference type 'NoRetainRelease' must have 'release:' Swift attribute
public func test(x: NoRetainRelease) { }

// CHECK: error: specified retain function 'badRetain' is invalid; retain function must either return have 'void', the reference count as an integer, or the parameter type
// CHECK: error: specified release function 'badRelease' is invalid; release function must have exactly one argument of type 'BadRetainRelease'
public func test(x: BadRetainRelease) { }
