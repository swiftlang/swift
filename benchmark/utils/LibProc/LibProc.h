//===--- LibProc.h --------------------------------------------------------===//
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

// For reasons I can't determine, passing a -target on the swift command line
// causes these headers' declarations (or at least the decl of proc_pid_rusage)
// to vanish.
//
// #include <sys/resource.h>
// #include <libproc.h>
//
// We therefore copy-and-paste the following declarations. Obviously this is wrong.

typedef void *rusage_info_t;
int proc_pid_rusage(int pid, int flavor, rusage_info_t *buffer) __attribute__((availability(macosx,introduced=10.9),availability(ios,introduced=7.0)));
