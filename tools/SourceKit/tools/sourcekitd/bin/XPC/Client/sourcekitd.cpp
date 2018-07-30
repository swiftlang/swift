//===--- sourcekitd.cpp ---------------------------------------------------===//
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

#include "sourcekitd/Internal-XPC.h"
#include "SourceKit/Support/Logging.h"
#include "SourceKit/Support/UIdent.h"

#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Mutex.h"
#include <chrono>
#include <xpc/xpc.h>
#include <dispatch/dispatch.h>

#include <Block.h>

using namespace SourceKit;
using namespace sourcekitd;

static UIdent gKeyNotification("key.notification");
static UIdent gKeyDuration("key.duration");
static UIdent gSemaDisableNotificationUID("source.notification.sema_disabled");

static llvm::sys::Mutex GlobalHandlersMtx;
static sourcekitd_uid_handler_t UidMappingHandler;
static sourcekitd_str_from_uid_handler_t StrFromUidMappingHandler;

void
sourcekitd_set_uid_handler(sourcekitd_uid_handler_t handler) {
  llvm::sys::ScopedLock L(GlobalHandlersMtx);
  sourcekitd_uid_handler_t newHandler = Block_copy(handler);
  Block_release(UidMappingHandler);
  UidMappingHandler = newHandler;
}

void
sourcekitd_set_uid_handlers(sourcekitd_uid_from_str_handler_t uid_from_str,
                            sourcekitd_str_from_uid_handler_t str_from_uid) {
  llvm::sys::ScopedLock L(GlobalHandlersMtx);

  sourcekitd_uid_handler_t newUIDFromStrHandler = Block_copy(uid_from_str);
  Block_release(UidMappingHandler);
  UidMappingHandler = newUIDFromStrHandler;

  sourcekitd_str_from_uid_handler_t newStrFromUIDHandler = Block_copy(str_from_uid);
  Block_release(StrFromUidMappingHandler);
  StrFromUidMappingHandler = newStrFromUIDHandler;
}

sourcekitd_uid_t sourcekitd::SKDUIDFromUIdent(UIdent UID) {
  if (void *Tag = UID.getTag())
    return reinterpret_cast<sourcekitd_uid_t>(Tag);

  if (UidMappingHandler) {
    sourcekitd_uid_t skduid = UidMappingHandler(UID.c_str());
    if (skduid) {
      UID.setTag(skduid);
      return skduid;
    }
  }

  return reinterpret_cast<sourcekitd_uid_t>(UID.getAsOpaqueValue());
}

UIdent sourcekitd::UIdentFromSKDUID(sourcekitd_uid_t uid) {
  if (StrFromUidMappingHandler) {
    if (const char *str = StrFromUidMappingHandler(uid))
      return UIdent(str);
  }

  return UIdent::getFromOpaqueValue(uid);
}

static xpc_connection_t GlobalConn = nullptr;
static sourcekitd_interrupted_connection_handler_t InterruptedConnectionHandler;
static sourcekitd_response_receiver_t NotificationReceiver;

void
sourcekitd_set_interrupted_connection_handler(
                          sourcekitd_interrupted_connection_handler_t handler) {
  sourcekitd_interrupted_connection_handler_t newHandler = Block_copy(handler);
  dispatch_async(dispatch_get_main_queue(), ^{
    Block_release(InterruptedConnectionHandler);
    InterruptedConnectionHandler = newHandler;
  });
}

void
sourcekitd_set_notification_handler(sourcekitd_response_receiver_t receiver) {
  sourcekitd_response_receiver_t newReceiver = Block_copy(receiver);
  dispatch_async(dispatch_get_main_queue(), ^{
    Block_release(NotificationReceiver);
    NotificationReceiver = newReceiver;
  });
}

//===----------------------------------------------------------------------===//
// sourcekitd_request_sync
//===----------------------------------------------------------------------===//

static xpc_connection_t getGlobalConnection();

static bool ConnectionInterrupted = false;

sourcekitd_response_t sourcekitd_send_request_sync(sourcekitd_object_t req) {
  LOG_SECTION("sourcekitd_send_request_sync-before", InfoHighPrio) {
    // Requests will be printed in Requests.cpp, print them out here as well.
    sourcekitd::printRequestObject(req, Log->getOS());
  }

  if (ConnectionInterrupted) {
    LOG_WARN_FUNC("request dropped while restoring service");
    return sourcekitd::createErrorRequestInterrupted("restoring service");
  }

  xpc_connection_t Conn = getGlobalConnection();
  xpc_object_t contents = xpc_array_create(nullptr, 0);
  xpc_array_append_value(contents, req);

  xpc_object_t msg = xpc_dictionary_create(nullptr, nullptr,  0);
  xpc_dictionary_set_value(msg, "msg", contents);
  xpc_release(contents);

  xpc_object_t reply = xpc_connection_send_message_with_reply_sync(Conn, msg);
  xpc_release(msg);
  if (xpc_get_type(reply) == XPC_TYPE_ERROR)
    return reply;

  assert(xpc_get_type(reply) == XPC_TYPE_DICTIONARY);
  sourcekitd_object_t resp =
    xpc_dictionary_get_value(reply, xpc::KeyMsgResponse);
  xpc_retain(resp);
  xpc_release(reply);

  LOG_SECTION("sourcekitd_send_request_sync-after", InfoHighPrio) {
    // Responses will be printed in Requests.cpp (with medium prio), print them
    // out here as well.
    if (Logger::isLoggingEnabledForLevel(Logger::Level::InfoMediumPrio))
      sourcekitd::printResponse(resp, Log->getOS());
  }

  return resp;
}

void sourcekitd_send_request(sourcekitd_object_t req,
                             sourcekitd_request_handle_t *out_handle,
                             sourcekitd_response_receiver_t receiver) {
  // FIXME: Implement request handle.

  LOG_SECTION("sourcekitd_send_request-before", InfoHighPrio) {
    // Requests will be printed in Requests.cpp, print them out here as well.
    sourcekitd::printRequestObject(req, Log->getOS());
  }

  if (ConnectionInterrupted) {
    LOG_WARN_FUNC("request dropped while restoring service");
    receiver(sourcekitd::createErrorRequestInterrupted("restoring service"));
    return;
  }

  xpc_connection_t Conn = getGlobalConnection();
  xpc_object_t contents = xpc_array_create(nullptr, 0);
  xpc_array_append_value(contents, req);

  xpc_object_t msg = xpc_dictionary_create(nullptr, nullptr,  0);
  xpc_dictionary_set_value(msg, "msg", contents);
  xpc_release(contents);

  dispatch_queue_t queue
    = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
  xpc_connection_send_message_with_reply(Conn, msg, queue,
                                         ^(xpc_object_t reply) {
    sourcekitd_response_t Resp = nullptr;
    if (xpc_get_type(reply) == XPC_TYPE_ERROR) {
      Resp = reply;
    } else {
      assert(xpc_get_type(reply) == XPC_TYPE_DICTIONARY);
      Resp = xpc_dictionary_get_value(reply, xpc::KeyMsgResponse);
    }
    // In sourcekitd, the receiver accepts ownership of the response.
    xpc_retain(Resp);

    LOG_SECTION("sourcekitd_send_request-after", InfoHighPrio) {
      // Responses will be printed in Requests.cpp (with medium prio),
      // print them out here as well.
      if (Logger::isLoggingEnabledForLevel(Logger::Level::InfoMediumPrio))
        sourcekitd::printResponse(Resp, Log->getOS());
    }

    receiver(Resp);
  });
  xpc_release(msg);
}

void sourcekitd_cancel_request(sourcekitd_request_handle_t handle) {
  // FIXME: Implement cancelling.
}

/// To avoid repeated crashes, used to notify the service to delay typechecking
/// in the editor for a certain amount of seconds.
static std::atomic<size_t> SemanticEditorDelaySecondsNum;

static void handleInternalInitRequest(xpc_object_t reply) {
  size_t Delay = SemanticEditorDelaySecondsNum;
  if (Delay != 0)
    xpc_dictionary_set_uint64(reply, xpc::KeySemaEditorDelay, Delay);
}

static void handleInternalUIDRequest(xpc_object_t XVal,
                                     xpc_object_t reply) {
  xpc_object_t response = nullptr;
  if (xpc_get_type(XVal) == XPC_TYPE_STRING) {
    const char *Str = xpc_string_get_string_ptr(XVal);
    LOG_INFO_FUNC(Low, "service queried UID for: " << Str);
    sourcekitd_uid_t SKDUID = sourcekitd_uid_get_from_cstr(Str);
    response = xpc_uint64_create(uintptr_t(SKDUID));

  } else if (xpc_get_type(XVal) == XPC_TYPE_UINT64) {
    uint64_t Val = xpc_uint64_get_value(XVal);
    sourcekitd_uid_t SKDUID = reinterpret_cast<sourcekitd_uid_t>(Val);
    const char *Str = sourcekitd_uid_get_string_ptr(SKDUID);
    LOG_INFO_FUNC(Low, "service queried string of UID: " << Str);
    response = xpc_string_create(Str);

  } else {
    llvm::report_fatal_error("Unknown internal message");
  }

  xpc_dictionary_set_value(reply, xpc::KeyMsgResponse, response);
  xpc_release(response);
}

static void handleInterruptedConnection(xpc_object_t event, xpc_connection_t conn);

void sourcekitd::initialize() {
  assert(!GlobalConn);
  GlobalConn = xpc_connection_create(SOURCEKIT_XPCSERVICE_IDENTIFIER, nullptr);

  xpc_connection_set_event_handler(GlobalConn, ^(xpc_object_t event) {
    xpc_type_t type = xpc_get_type(event);

    if (type == XPC_TYPE_ERROR) {
      if (event == XPC_ERROR_CONNECTION_INTERRUPTED) {
        // The service has crashed. The XPC connection is still valid and
        // sending a message to it will re-launch the service.
        LOG_WARN("connection-event-handler", "Connection interrupt");
        handleInterruptedConnection(event, GlobalConn);

      } else if (event == XPC_ERROR_CONNECTION_INVALID) {
        // Client initiated shutdown.
        LOG_INFO("connection-event-handler", High,
                 "connection invalid error (shutdown)");
        xpc_release(GlobalConn);
        GlobalConn = nullptr;

      } else {
        LOG_WARN("connection-event-handler",
                 "Received unexpected error event: " <<
                 xpc_dictionary_get_string(event, XPC_ERROR_KEY_DESCRIPTION));

      }
    } else {
      xpc_object_t contents = xpc_dictionary_get_value(event, xpc::KeyInternalMsg);
      if (!contents) {
        llvm::report_fatal_error("Received unexpected message from service");
      }

      xpc::Message msg = (xpc::Message)xpc_array_get_uint64(contents, 0);
      switch (msg) {
      case xpc::Message::Initialization: {
        xpc_object_t reply = xpc_dictionary_create_reply(event);
        handleInternalInitRequest(reply);
        xpc_connection_send_message(GlobalConn, reply);
        xpc_release(reply);
        break;
      }

      case xpc::Message::Notification: {
        xpc_object_t notif_contents = xpc_retain(xpc_array_get_value(contents, 1));
        dispatch_async(dispatch_get_main_queue(), ^{
          if (NotificationReceiver != nullptr) {
            // The receiver accepts ownership of the notification object.
            NotificationReceiver(xpc_retain(notif_contents));
          }
          xpc_release(notif_contents);
        });
        break;
      }

      case xpc::Message::UIDSynchronization: {
        xpc_object_t reply = xpc_dictionary_create_reply(event);
        handleInternalUIDRequest(xpc_array_get_value(contents, 1), reply);
        xpc_connection_send_message(GlobalConn, reply);
        xpc_release(reply);
        break;
      }
      }
    }
  });

  xpc_connection_resume(GlobalConn);
}

void sourcekitd::shutdown() {
  assert(GlobalConn);
  xpc_connection_cancel(GlobalConn);
}

static xpc_connection_t getGlobalConnection() {
  assert(GlobalConn);
  return GlobalConn;
}

/// Receives a +1 reference of the connection.
static void pingService(xpc_connection_t ping_conn) {
  LOG_WARN_FUNC("pinging service");

  xpc_object_t ping_msg = xpc_dictionary_create(nullptr, nullptr, 0);
  xpc_dictionary_set_bool(ping_msg, "ping", true);

  dispatch_queue_t queue
    = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
  xpc_connection_send_message_with_reply(ping_conn, ping_msg, queue,
                                         ^(xpc_object_t reply) {
    xpc_type_t type = xpc_get_type(reply);

    if (type == XPC_TYPE_ERROR) {
      if (reply == XPC_ERROR_CONNECTION_INTERRUPTED) {
        // Try again.
        pingService(ping_conn);

      } else if (reply == XPC_ERROR_CONNECTION_INVALID) {
        // Client initiated shutdown.
        LOG_WARN("ping-event-handler", "connection invalid error");
        xpc_release(ping_conn);

      } else {
        LOG_WARN("ping-event-handler",
                 "Received unexpected error reply: " <<
                 xpc_dictionary_get_string(reply, XPC_ERROR_KEY_DESCRIPTION));
        // Try again.
        pingService(ping_conn);

      }

      return;
    }

    LOG_WARN("ping-event-handler", "service restored");
    ConnectionInterrupted = false;

    // Create an empty response as notification that the service is restored.
    sourcekitd::ResponseBuilder RespBuilder;
    xpc_object_t contents = RespBuilder.createResponse();
    dispatch_async(dispatch_get_main_queue(), ^{
      if (NotificationReceiver != nullptr) {
        // The receiver accepts ownership of the notification object.
        xpc_object_t notif_contents = xpc_retain(contents);
        NotificationReceiver(notif_contents);
      }
      xpc_release(contents);
    });

    xpc_release(ping_conn);
  });

  xpc_release(ping_msg);
}

static void sendNotification(xpc_object_t event) {
  event = xpc_retain(event);
  dispatch_async(dispatch_get_main_queue(), ^{
    if (NotificationReceiver != nullptr) {
      // The receiver accepts ownership of the notification object.
      xpc_object_t err_event = xpc_retain(event);
      NotificationReceiver(err_event);
    }
    xpc_release(event);
  });
}

static void updateSemanticEditorDelay() {
  using namespace std::chrono;
  using TimePoint = time_point<system_clock, nanoseconds>;

  // This minimum is chosen to keep us from being throttled by XPC.
  static const size_t MinDelaySeconds = 10;
  static const size_t MaxDelaySeconds = 20;

  // Clear any previous setting.
  SemanticEditorDelaySecondsNum = 0;

  // Leave the delay at 0 if it is explicitly disabled
  if(::getenv("SOURCEKIT_DISABLE_SEMA_EDITOR_DELAY"))
    return;

  static TimePoint gPrevCrashTime;

  TimePoint PrevTime = gPrevCrashTime;
  TimePoint CurrTime = system_clock::now();
  gPrevCrashTime = CurrTime;

  auto Diff = duration_cast<seconds>(CurrTime - PrevTime);
  size_t Delay = Diff.count()*2 + 1;
  if (Diff.count() > 30)
    Delay = 0; // Treat this as more likely unrelated to the previous crash.
  Delay = std::min(std::max(Delay, MinDelaySeconds), MaxDelaySeconds);

  LOG_WARN_FUNC("disabling semantic editor for " << Delay << " seconds");
  SemanticEditorDelaySecondsNum = Delay;

  // Notify the client that semantic functionality is disabled.
  ResponseBuilder RespBuilder;
  auto Dict = RespBuilder.getDictionary();
  Dict.set(gKeyNotification, gSemaDisableNotificationUID);
  Dict.set(gKeyDuration, Delay);
  sourcekitd_response_t SemaDisableNotification = RespBuilder.createResponse();
  sendNotification(SemaDisableNotification);
  sourcekitd_response_dispose(SemaDisableNotification);
}

static void handleInterruptedConnection(xpc_object_t event, xpc_connection_t conn) {
  ConnectionInterrupted = true;

  updateSemanticEditorDelay();

  // FIXME: InterruptedConnectionHandler will go away.
  dispatch_async(dispatch_get_main_queue(), ^{
    if (InterruptedConnectionHandler != nullptr) {
      InterruptedConnectionHandler();
    }
  });
  // Send the disconnect error as notification.
  sendNotification(event);

  // Retain connection while we try to ping it.
  // Since this happens implicitly, we can't blame the client if it shuts down
  // while we are trying to ping.
  pingService((xpc_connection_t)xpc_retain(conn));
}
