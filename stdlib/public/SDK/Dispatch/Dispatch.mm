//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <dispatch/dispatch.h>
#include <objc/runtime.h>
#include <stdio.h>

@protocol OS_dispatch_source;
@protocol OS_dispatch_source_mach_send;
@protocol OS_dispatch_source_mach_recv;
@protocol OS_dispatch_source_memorypressure;
@protocol OS_dispatch_source_proc;
@protocol OS_dispatch_source_read;
@protocol OS_dispatch_source_signal;
@protocol OS_dispatch_source_timer;
@protocol OS_dispatch_source_data_add;
@protocol OS_dispatch_source_data_or;
@protocol OS_dispatch_source_vnode;
@protocol OS_dispatch_source_write;

// #include <dispatch/private.h>
__attribute__((constructor))
static void _dispatch_overlay_constructor() {
  Class source = objc_lookUpClass("OS_dispatch_source");
  if (source) {
    class_addProtocol(source, @protocol(OS_dispatch_source));
    class_addProtocol(source, @protocol(OS_dispatch_source_mach_send));
    class_addProtocol(source, @protocol(OS_dispatch_source_mach_recv));
    class_addProtocol(source, @protocol(OS_dispatch_source_memorypressure));
    class_addProtocol(source, @protocol(OS_dispatch_source_proc));
    class_addProtocol(source, @protocol(OS_dispatch_source_read));
    class_addProtocol(source, @protocol(OS_dispatch_source_signal));
    class_addProtocol(source, @protocol(OS_dispatch_source_timer));
    class_addProtocol(source, @protocol(OS_dispatch_source_data_add));
    class_addProtocol(source, @protocol(OS_dispatch_source_data_or));
    class_addProtocol(source, @protocol(OS_dispatch_source_vnode));
    class_addProtocol(source, @protocol(OS_dispatch_source_write));
  }
}

