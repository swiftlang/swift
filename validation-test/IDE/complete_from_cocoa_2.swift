// RUN: %swift-ide-test -code-completion -source-filename %s -sdk %sdk -code-completion-token=T1 | FileCheck %s -check-prefix=T1
// RUN: %swift-ide-test -code-completion -source-filename %s -sdk %sdk -code-completion-token=T2 | FileCheck %s -check-prefix=T2

// REQUIRES: objc_interop

// FIXME: iOS has no Cocoa.framework
// REQUIRES: OS=macosx

// A smoketest for code completion in Cocoa.

import Cocoa

func testQualifiedWithDot() {
  Cocoa.#^T1^#
// T1: Begin completions
// T1-DAG: Decl[FreeFunction]/OtherModule: CFArrayCreate({#(allocator): CFAllocator!#}, {#(values): UnsafeMutablePointer<UnsafePointer<Void>>#}, {#(numValues): CFIndex#}, {#(callBacks): UnsafePointer<CFArrayCallBacks>#})[#CFArray!#]{{$}}
// T1-DAG: Decl[FreeFunction]/OtherModule: CFArrayGetCount({#(theArray): CFArray!#})[#CFIndex#]{{$}}
// T1-DAG: Decl[Class]/OtherModule:        NSObject[#NSObject#]{{$}}
// T1: End completions
}

func testQualifiedWithoutDot() {
  Cocoa#^T2^#
// T2: Begin completions
// T2-DAG: Decl[FreeFunction]/OtherModule: .CFArrayCreate({#(allocator): CFAllocator!#}, {#(values): UnsafeMutablePointer<UnsafePointer<Void>>#}, {#(numValues): CFIndex#}, {#(callBacks): UnsafePointer<CFArrayCallBacks>#})[#CFArray!#]{{$}}
// T2-DAG: Decl[FreeFunction]/OtherModule: .CFArrayGetCount({#(theArray): CFArray!#})[#CFIndex#]{{$}}
// T2-DAG: Decl[Class]/OtherModule:        .NSObject[#NSObject#]{{$}}
// T2: End completions
}

