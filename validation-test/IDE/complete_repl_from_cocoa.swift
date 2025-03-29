// RUN: %target-swift-ide-test -repl-code-completion -source-filename %s | %FileCheck %s

// A smoketest for REPL code completion in Cocoa.

// REQUIRES: objc_interop
// FIXME: iOS has no Cocoa.framework
// REQUIRES: OS=macosx

// CHECK: {{^}}CFArrayCreateCopy(allocator: CFAllocator!, theArray: CFArray!) -> CFArray!{{$}}
// CHECK: {{^}}FE_ALL_EXCEPT: Int32{{$}}

import Cocoa

