//===--- Internal-XPC.h - ---------------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKITD_INTERNAL_XPC_H
#define LLVM_SOURCEKITD_INTERNAL_XPC_H

#include "sourcekitd/Internal.h"

namespace sourcekitd {
namespace xpc {

static const char *KeyInternalMsg = "internal_msg";
static const char *KeySemaEditorDelay = "semantic_editor_delay";
static const char *KeyTracingEnabled = "tracing_enabled";
static const char *KeyMsgResponse = "response";

enum class Message {
  Initialization,
  Notification,
  UIDSynchronization,
};

} // namespace xpc
} // namespace sourcekitd

#endif
