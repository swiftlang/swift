// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=T1 | %FileCheck %s -check-prefix=T1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=T2 | %FileCheck %s -check-prefix=T2

// REQUIRES: objc_interop

// FIXME: iOS has no Cocoa.framework
// REQUIRES: OS=macosx
// A smoketest for code completion in Cocoa.

import Cocoa

func testQualifiedWithDot() {
  Cocoa.#^T1^#
// T1-DAG: Decl[FreeFunction]/OtherModule[CoreFoundation.CFArray]/IsSystem: CFArrayCreate({#(allocator): CFAllocator!#}, {#(values): UnsafeMutablePointer<UnsafeRawPointer?>!#}, {#(numValues): CFIndex#}, {#(callBacks): UnsafePointer<CFArrayCallBacks>!#})[#CFArray!#]{{; name=.+$}}
// T1-DAG: Decl[FreeFunction]/OtherModule[CoreFoundation.CFArray]/IsSystem: CFArrayGetCount({#(theArray): CFArray!#})[#CFIndex#]{{; name=.+$}}
// T1-DAG: Decl[Class]/OtherModule[ObjectiveC.NSObject]/IsSystem:        NSObject[#NSObject#]{{; name=.+$}}
}

func testQualifiedWithoutDot() {
  Cocoa#^T2^#
// T2-DAG: Decl[FreeFunction]/OtherModule[CoreFoundation.CFArray]/IsSystem: .CFArrayCreate({#(allocator): CFAllocator!#}, {#(values): UnsafeMutablePointer<UnsafeRawPointer?>!#}, {#(numValues): CFIndex#}, {#(callBacks): UnsafePointer<CFArrayCallBacks>!#})[#CFArray!#]{{; name=.+$}}
// T2-DAG: Decl[FreeFunction]/OtherModule[CoreFoundation.CFArray]/IsSystem: .CFArrayGetCount({#(theArray): CFArray!#})[#CFIndex#]{{; name=.+$}}
// T2-DAG: Decl[Class]/OtherModule[ObjectiveC.NSObject]/IsSystem:        .NSObject[#NSObject#]{{; name=.+$}}
}
