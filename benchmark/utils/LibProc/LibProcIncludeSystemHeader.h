//===--- LibProcIncludeSystemHeader.h -------------------------------------===//
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

// This file exists to include the not-yet-modularized libproc.h system header.

#if __has_include(<libproc.h>)
#include <libproc.h>
#else
#include <Availability.h>
#include <sys/resource.h>
// Some SDKs are missing the libproc.h header, despite this symbol being present.
int proc_pid_rusage(int pid, int flavor, rusage_info_t *buffer) __OSX_AVAILABLE_STARTING(__MAC_10_9, __IPHONE_7_0);
#endif
