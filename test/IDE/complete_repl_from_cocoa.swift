// RUN: %swift-ide-test -repl-code-completion -source-filename %s | FileCheck %s

// A smoketest for REPL code completion in Cocoa.

// REQUIRES: sdk
// REQUIRES: long_tests

// FIXME: iOS has no Cocoa.framework
// REQUIRES: OS=macosx

// CHECK: {{^}}CFArrayCreateCopy(allocator: CFAllocator?, theArray: CFArray?) -> CFArray!{{$}}
// CHECK: {{^}}FE_ALL_EXCEPT: CInt{{$}}

import Cocoa

