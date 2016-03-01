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

static uint64_t extractUInt64(mach_msg_type_descriptor_t type_desc) {
  return (type_desc.pad1 + ((uint64_t)type_desc.pad2 << 32));
}

static void packUInt64(uint64_t value,
                          mach_msg_type_descriptor_t *type_desc) {
  type_desc->pad1 = (uint32_t)value;
  type_desc->pad2 = value >> 32;
}

static mach_port_t getSelfTask() {
  return mach_task_self();
}

static void guardMachError(kern_return_t error, const char *message) {
  if (error == KERN_SUCCESS) return;
  fprintf(stderr, "%s failed: %s\n", message, mach_error_string(error));
  exit(EXIT_FAILURE);
}

/// Sends `portToSend` through `remotePort`.
static void sendPort(mach_port_t portToSend, mach_port_t remotePort) {
  kern_return_t error;

  SendPortMessage message;

  message.header.msgh_remote_port = remotePort;
  message.header.msgh_local_port = MACH_PORT_NULL;
  message.header.msgh_bits = MACH_MSGH_BITS (MACH_MSG_TYPE_COPY_SEND, 0) |
  MACH_MSGH_BITS_COMPLEX;
  message.header.msgh_size = sizeof(message);

  message.body.msgh_descriptor_count = 1;
  message.task_port.name = portToSend;
  message.task_port.disposition = MACH_MSG_TYPE_COPY_SEND;
  message.task_port.type = MACH_MSG_PORT_DESCRIPTOR;

  error = mach_msg_send (&message.header);
  guardMachError(error, "mach_msg_send");
}

static mach_port_t getSelfBootstrapPort() {
  mach_port_t port;
  kern_return_t error = task_get_bootstrap_port(mach_task_self(), &port);
  guardMachError(error, "task_get_bootstrap_port");
  return port;
}

static void sendUInt64(uint64_t value, mach_port_t remotePort) {
  SendUInt64Message message;
  message.header.msgh_remote_port = remotePort;
  message.header.msgh_local_port = MACH_PORT_NULL;
  message.header.msgh_bits = MACH_MSGH_BITS(MACH_MSG_TYPE_COPY_SEND, 0);
  message.header.msgh_size = sizeof(message);

  message.body.msgh_descriptor_count = 1;
  message.value.pad1 = (uint32_t)value;
  message.value.pad2 = value >> 32;

  kern_return_t error = mach_msg_send(&message.header);
  guardMachError(error, "mach_msg_send");
}

static uint64_t receive_uint64_t(mach_port_t from_port) {
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

static void sendReflectionInfo(RemoteReflectionInfo info, mach_port_t remotePort) {
  SendReflectionInfoMessage message;
  message.header.msgh_remote_port = remotePort;
  message.header.msgh_local_port = MACH_PORT_NULL;
  message.header.msgh_bits = MACH_MSGH_BITS(MACH_MSG_TYPE_COPY_SEND, 0);
  message.header.msgh_size = sizeof(message);

  message.body.msgh_descriptor_count = 9;

  packUInt64(info.image_name_addr, &message.image_name_addr);
  packUInt64(info.fieldmd.addr, &message.fieldmd_start_addr);
  packUInt64(info.fieldmd.size, &message.fieldmd_size);
  packUInt64(info.typeref.addr, &message.typeref_start_addr);
  packUInt64(info.typeref.size, &message.typeref_size);
  packUInt64(info.reflstr.addr, &message.reflstr_start_addr);
  packUInt64(info.reflstr.size, &message.reflstr_size);
  packUInt64(info.assocty.addr, &message.assocty_start_addr);
  packUInt64(info.assocty.size, &message.assocty_size);

  kern_return_t error = mach_msg_send(&message.header);
  guardMachError(error, "mach_msg_send");
}

#endif // SWIFT_REFLECTION_TEST_MACH_HELPERS_H

