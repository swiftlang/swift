// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx12.0 -target-sdk-version 15.5 -c %s -o %t/old.o
// RUN: %llvm-nm %t/old.o | %FileCheck %s --check-prefix=OLD-SDK
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx12.0 -target-sdk-version 26.0 -c %s -o %t/new.o
// RUN: %llvm-nm %t/new.o | %FileCheck %s --check-prefix=NEW-SDK

// rdar://189824737: When the deployment target predates swift_coroFrameAlloc,
// coroutine frames are allocated through a stub that weakly references it.
// On Darwin, the static linker must still find that symbol in the SDK's Swift
// runtime, so if the SDK predates it (15.5 here), the stub must use malloc and
// not reference swift_coroFrameAlloc at all. With a newer SDK (26.0), the weak
// reference is kept.
//
// The _modify yields through Dictionary's own modify coroutine, so its frame
// doesn't fit in the caller's buffer and must be allocated.

public struct S {
  var d: [Int: String] = [:]
  public subscript(k: Int) -> String? {
    get { d[k] }
    _modify { yield &d[k] }
  }
}

// OLD-SDK-NOT: _swift_coroFrameAlloc{{$}}
// NEW-SDK: U _swift_coroFrameAlloc{{$}}
