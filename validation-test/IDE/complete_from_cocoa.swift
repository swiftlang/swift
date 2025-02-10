// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=T1 | %FileCheck %s -check-prefix=T1

// REQUIRES: objc_interop
// REQUIRES: no_asan

// FIXME: iOS has no Cocoa.framework
// REQUIRES: OS=macosx
// https://github.com/swiftlang/swift/issues/79255
// REQUIRES: rdar141124373
// A smoketest for code completion in Cocoa.

import Cocoa

func testUnqualified() {
  #^T1^#
// T1-DAG: Decl[FreeFunction]/OtherModule[CoreFoundation.CFArray]/IsSystem: CFArrayCreate({#(allocator): CFAllocator!#}, {#(values): UnsafeMutablePointer<UnsafeRawPointer?>!#}, {#(numValues): CFIndex#}, {#(callBacks): UnsafePointer<CFArrayCallBacks>!#})[#CFArray!#]{{; name=.+$}}
// T1-DAG: Decl[FreeFunction]/OtherModule[CoreFoundation.CFArray]/IsSystem: CFArrayGetCount({#(theArray): CFArray!#})[#CFIndex#]{{; name=.+$}}
// T1-DAG: Decl[Class]/OtherModule[ObjectiveC.NSObject]/IsSystem:        NSObject[#NSObject#]{{; name=.+$}}
}
