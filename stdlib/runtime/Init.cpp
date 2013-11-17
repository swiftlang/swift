//===--- Init.cpp - Runtime initialization ---------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift runtime initialization.
//
//===----------------------------------------------------------------------===//

#include <mach/mach.h>
#include <mach/task.h>
#include <cassert>

__attribute__((constructor))
static void init() {
#if defined (__ppc__) || defined(__ppc64__)
  thread_state_flavor_t flavor = PPC_THREAD_STATE64;
#elif defined(__i386__) || defined(__x86_64__)
  thread_state_flavor_t flavor = x86_THREAD_STATE;
#elif defined(__arm__) || defined(__arm64__)
  thread_state_flavor_t flavor = ARM_THREAD_STATE;
#else
#error "unknown architecture"
#endif

  kern_return_t KR = task_set_exception_ports(mach_task_self(), EXC_MASK_CRASH,
                                              MACH_PORT_NULL,
                                              EXCEPTION_STATE_IDENTITY
                                              | MACH_EXCEPTION_CODES, flavor);
  assert(KR == 0);
}
