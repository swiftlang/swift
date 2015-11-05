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
  TraceMessage
};

} // namespace xpc
} // namespace sourcekitd

#endif
