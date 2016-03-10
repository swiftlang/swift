//===--- mach_messages.h - Reflection testing: Mach Messages --------------===//
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
// This file defines some Mach message structures for interacting with a
// child process on OS X during reflection tests.
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_TEST_MACH_MESSAGES_H
#define SWIFT_REFLECTION_TEST_MACH_MESSAGES_H

#include <mach/mach.h>

typedef struct Section {
  uint64_t addr;
  uint64_t size;
} Section;

typedef struct RemoteReflectionInfo {
  uint64_t image_name_addr;
  Section fieldmd;
  Section typeref;
  Section reflstr;
  Section assocty;
} RemoteReflectionInfo;

typedef struct SendPortMessage {
  mach_msg_header_t header;
  mach_msg_body_t body;
  mach_msg_port_descriptor_t task_port;
} SendPortMessage;

typedef struct ReceivePortMessage {
  mach_msg_header_t header;
  mach_msg_body_t body;
  mach_msg_port_descriptor_t taskPort;
  mach_msg_trailer_t trailer;
} ReceivePortMessage;

typedef struct SendUInt64Message {
  mach_msg_header_t header;
  mach_msg_body_t body;
  mach_msg_type_descriptor_t value;
} SendUInt64Message;

typedef struct ReceiveUInt64Message {
  mach_msg_header_t header;
  mach_msg_body_t body;
  mach_msg_type_descriptor_t value;
  mach_msg_trailer_t trailer;
} ReceiveUInt64Message;

typedef struct SendReflectionInfoMessage {
  mach_msg_header_t header;
  mach_msg_body_t body;
  mach_msg_type_descriptor_t image_name_addr;
  mach_msg_type_descriptor_t fieldmd_start_addr;
  mach_msg_type_descriptor_t fieldmd_size;
  mach_msg_type_descriptor_t typeref_start_addr;
  mach_msg_type_descriptor_t typeref_size;
  mach_msg_type_descriptor_t reflstr_start_addr;
  mach_msg_type_descriptor_t reflstr_size;
  mach_msg_type_descriptor_t assocty_start_addr;
  mach_msg_type_descriptor_t assocty_size;
} SendReflectionInfoMessage;

typedef struct ReceiveReflectionInfoMessage {
  mach_msg_header_t header;
  mach_msg_body_t body;
  mach_msg_type_descriptor_t image_name_addr;
  mach_msg_type_descriptor_t fieldmd_start_addr;
  mach_msg_type_descriptor_t fieldmd_size;
  mach_msg_type_descriptor_t typeref_start_addr;
  mach_msg_type_descriptor_t typeref_size;
  mach_msg_type_descriptor_t reflstr_start_addr;
  mach_msg_type_descriptor_t reflstr_size;
  mach_msg_type_descriptor_t assocty_start_addr;
  mach_msg_type_descriptor_t assocty_size;
  mach_msg_trailer_t trailer;
} ReceiveReflectionInfoMessage;


#endif /* SWIFT_REFLECTION_TEST_MACH_MESSAGES_H */

