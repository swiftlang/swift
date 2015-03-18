// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=T1 | FileCheck %s -check-prefix=T1

// REQUIRES: objc_interop

// FIXME: iOS has no Cocoa.framework
// REQUIRES: OS=macosx

// A smoketest for code completion in Cocoa.

import Cocoa

func testUnqualified() {
  #^T1^#
// T1: Begin completions
// T1-DAG: Decl[FreeFunction]/OtherModule[CoreFoundation.CFArray]: CFArrayCreate({#(allocator): CFAllocator!#}, {#(values): UnsafeMutablePointer<UnsafePointer<Void>>#}, {#(numValues): CFIndex#}, {#(callBacks): UnsafePointer<CFArrayCallBacks>#})[#CFArray!#]{{; name=.+$}}
// T1-DAG: Decl[FreeFunction]/OtherModule[CoreFoundation.CFArray]: CFArrayGetCount({#(theArray): CFArray!#})[#CFIndex#]{{; name=.+$}}
// T1-DAG: Decl[Class]/OtherModule[ObjectiveC.NSObject]:        NSObject[#NSObject#]{{; name=.+$}}
// T1: End completions
}
