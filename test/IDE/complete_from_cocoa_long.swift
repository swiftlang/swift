// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift-ide-test -code-completion -source-filename %s -module-cache-path=%t/clang-module-cache -code-completion-token=T1 | FileCheck %s -check-prefix=T1
// RUN: %swift-ide-test -code-completion -source-filename %s -module-cache-path=%t/clang-module-cache -code-completion-token=T2 | FileCheck %s -check-prefix=T2

// REQUIRES: sdk
// REQUIRES: long_tests

// A smoketest for code completion in Cocoa.

import Cocoa

def testQualifiedWithDot() {
  Cocoa.#^T1^#
// T1: Begin completions
// T1-DAG: Decl[FreeFunction]/OtherModule: CFArrayCreate({#allocator: CFAllocatorRef#}, {#values: UnsafePointer<COpaquePointer>#}, {#numValues: CFIndex#}, {#callBacks: UnsafePointer<CFArrayCallBacks>#})[#CFArrayRef#]{{$}}
// T1-DAG: Decl[FreeFunction]/OtherModule: CFArrayGetCount({#theArray: CFArrayRef#})[#CFIndex#]{{$}}
// T1-DAG: Decl[Class]/OtherModule:        NSObject[#NSObject.metatype#]{{$}}
// T1: End completions
}

def testQualifiedWithoutDot() {
  Cocoa#^T2^#
// T2: Begin completions
// T2-DAG: Decl[FreeFunction]/OtherModule: .CFArrayCreate({#allocator: CFAllocatorRef#}, {#values: UnsafePointer<COpaquePointer>#}, {#numValues: CFIndex#}, {#callBacks: UnsafePointer<CFArrayCallBacks>#})[#CFArrayRef#]{{$}}
// T2-DAG: Decl[FreeFunction]/OtherModule: .CFArrayGetCount({#theArray: CFArrayRef#})[#CFIndex#]{{$}}
// T2-DAG: Decl[Class]/OtherModule:        .NSObject[#NSObject.metatype#]{{$}}
// T2: End completions
}

