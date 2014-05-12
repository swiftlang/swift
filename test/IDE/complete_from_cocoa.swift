// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift-ide-test -code-completion -source-filename %s -module-cache-path %t/clang-module-cache -sdk %sdk  -code-completion-token=T1 | FileCheck %s -check-prefix=T1

// REQUIRES: sdk
// REQUIRES: long_tests

// FIXME: iOS has no Cocoa.framework
// REQUIRES: OS=macosx

// A smoketest for code completion in Cocoa.

import Cocoa

func testUnqualified() {
  #^T1^#
// T1: Begin completions
// T1-DAG: Decl[FreeFunction]/OtherModule: CFArrayCreate({#CFAllocator?#}, {#CMutablePointer<COpaquePointer>#}, {#CFIndex#}, {#CConstPointer<CFArrayCallBacks>#})[#CFArray!#]{{$}}
// T1-DAG: Decl[FreeFunction]/OtherModule: CFArrayGetCount({#CFArray?#})[#CFIndex#]{{$}}
// T1-DAG: Decl[Class]/OtherModule:        NSObject[#NSObject#]{{$}}
// T1: End completions
}

