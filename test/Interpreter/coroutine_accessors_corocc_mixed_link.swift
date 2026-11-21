// Objects compiled with and without swiftcorocc can be linked into one image.
//
// The coroutine allocation helpers (`_swift_coro_alloc` and friends) are weak,
// hidden definitions emitted into every object that needs them.  The
// swiftcorocc variants use a different calling convention and handle a null
// allocator (stack allocation with a popless return), so they must not share a
// name with the plain variants.  Otherwise the static linker keeps whichever
// definition it sees first, and a swiftcorocc coroutine can end up calling a
// helper that dereferences its null allocator.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Library built without swiftcorocc.  It uses no coroutines itself; it only
// contributes its copy of the helpers.  Linked first, so its copies win any
// name collision.
// RUN: %target-swift-frontend -c -parse-as-library -module-name Library \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -enable-callee-allocated-coro-abi -disable-arm64-corocc \
// RUN:     -emit-module-path %t/Library.swiftmodule \
// RUN:     %t/Library.swift -o %t/Library.o

// Client built with swiftcorocc.  Its own coroutine needs a dynamic allocation
// (a copy of a value of unknown size), which goes through the allocation helper
// with a null allocator.
// RUN: %target-swift-frontend -c -module-name Client \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -enable-callee-allocated-coro-abi -enable-arm64-corocc \
// RUN:     -I %t %t/Client.swift -o %t/Client.o

// RUN: %target-build-swift %t/Library.o %t/Client.o -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: CPU=arm64 || CPU=arm64e
// REQUIRES: swift_feature_CoroutineAccessors

//--- Library.swift

public struct Box<T> {
  public var v: T
  public init(_ v: T) { self.v = v }
  public var p: T {
    yielding borrow { let copy = v; yield copy }
    yielding mutate { var copy = v; yield &copy; v = copy }
  }
}

//--- Client.swift

import Library

struct LocalBox<T> {
  var v: T
  var p: T {
    get { v }
    yielding mutate { var copy = v; yield &copy; v = copy }
  }
}

@inline(never) func mutate(_ b: inout LocalBox<String>) { b.p.append("!") }

// Keep Library.o linked in.
print(Box("library").v)
// CHECK: library

var b = LocalBox(v: "client")
mutate(&b)
print(b.v)
// CHECK: client!
