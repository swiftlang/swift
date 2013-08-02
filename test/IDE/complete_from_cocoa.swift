// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=T1 | FileCheck %s -check-prefix=T1

// REQUIRES: sdk

// A smoketest for code completion in Cocoa.

import Cocoa

func testUnqualified() {
  #^T1^#
// T1: Begin completions
// T1-DAG: SwiftDecl: CFArrayCreate({#allocator: CFAllocatorRef#}, {#values: UnsafePointer<COpaquePointer>#}, {#numValues: CFIndex#}, {#callBacks: UnsafePointer<CFArrayCallBacks>#})[#CFArrayRef#]{{$}}
// T1-DAG: SwiftDecl: CFArrayGetCount({#theArray: CFArrayRef#})[#CFIndex#]{{$}}
// T1-DAG: SwiftDecl: NSObject[#NSObject.metatype#]
// T1: End completions
}

