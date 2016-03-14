//===--- mach_helpers.h - Reflection testing: Mach Messages ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines some helper functions for connecting to a child process
// on OS X in order to read directly from its memory during reflection tests.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_TEST_MACH_HELPERS_H
#define SWIFT_REFLECTION_TEST_MACH_HELPERS_H

#include <mach/mach.h>
#include <stdio.h>
#include <stdlib.h>

#include "mach_messages.h"

static inline uint64_t extractUInt64(mach_msg_type_descriptor_t type_desc) {
  return (type_desc.pad1 + ((uint64_t)type_desc.pad2 << 32));
}

static inline void guardMachError(kern_return_t error, const char *message) {
  if (error == KERN_SUCCESS) return;
  fprintf(stderr, "%s failed: %s\n", message, mach_error_string(error));
  abort();
}

static inline uint64_t receive_uint64_t(mach_port_t from_port) {
  ReceiveUInt64Message message;
  kern_return_t error = mach_msg(&message.header,
                                 MACH_RCV_MSG | MACH_RCV_INTERRUPT,
                                 0,
                                 sizeof(message),
                                 from_port,
                                 MACH_MSG_TIMEOUT_NONE,
                                 MACH_PORT_NULL);

  guardMachError(error, "mach_msg (MACH_RCV_MSG)");
  return extractUInt64(message.value);
}

#endif // SWIFT_REFLECTION_TEST_MACH_HELPERS_H

