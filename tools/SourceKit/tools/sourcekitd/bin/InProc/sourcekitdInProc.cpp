//===--- sourcekitdInProc.cpp ---------------------------------------------===//
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

#include "sourcekitd/Internal.h"

#include "SourceKit/Support/Concurrency.h"
#include "SourceKit/Support/UIdent.h"

#include "llvm/Support/Mutex.h"
#include "llvm/Support/Path.h"

// FIXME: Portability ?
#include <Block.h>

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#else
#include <dlfcn.h>
#endif

using namespace SourceKit;

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
  if (StrFromUidMappingHandler)
    return UIdent(StrFromUidMappingHandler(uid));

  return UIdent::getFromOpaqueValue(uid);
}

std::string sourcekitd::getRuntimeLibPath() {
#if defined(_WIN32)
  MEMORY_BASIC_INFORMATION mbi;
  char path[MAX_PATH + 1];
  if (!VirtualQuery(static_cast<void *>(sourcekitd_initialize), &mbi,
                    sizeof(mbi)))
    llvm_unreachable("call to VirtualQuery failed");
  if (!GetModuleFileNameA(static_cast<HINSTANCE>(mbi.AllocationBase), path,
                          MAX_PATH))
    llvm_unreachable("call to GetModuleFileNameA failed");
  return llvm::sys::path::parent_path(path);
#else
  // This silly cast below avoids a C++ warning.
  Dl_info info;
  if (dladdr((void *)(uintptr_t)sourcekitd_initialize, &info) == 0)
    llvm_unreachable("Call to dladdr() failed");

  // We now have the path to the shared lib, move to the parent 'lib' path.
  return llvm::sys::path::parent_path(info.dli_fname);
#endif
}

void sourcekitd::set_interrupted_connection_handler(
                          llvm::function_ref<void()> handler) {
}

//===----------------------------------------------------------------------===//
// sourcekitd_request_sync
//===----------------------------------------------------------------------===//

sourcekitd_response_t sourcekitd_send_request_sync(sourcekitd_object_t req) {
  Semaphore sema(0);

  sourcekitd_response_t ReturnedResp;
  sourcekitd::handleRequest(req, [&](sourcekitd_response_t resp) {
    ReturnedResp = resp;
    sema.signal();
  });

  sema.wait();
  return ReturnedResp;
}

void sourcekitd_send_request(sourcekitd_object_t req,
                             sourcekitd_request_handle_t *out_handle,
                             sourcekitd_response_receiver_t receiver) {
  // FIXME: Implement request handle.

  sourcekitd_request_retain(req);
  receiver = Block_copy(receiver);
  WorkQueue::dispatchConcurrent([=]{
    sourcekitd::handleRequest(req, [=](sourcekitd_response_t resp) {
      // The receiver accepts ownership of the response.
      receiver(resp);
      Block_release(receiver);
    });
    sourcekitd_request_release(req);
  });
}

void sourcekitd_cancel_request(sourcekitd_request_handle_t handle) {
  // FIXME: Implement cancelling.
}

void
sourcekitd_set_interrupted_connection_handler(
                          sourcekitd_interrupted_connection_handler_t handler) {
  // This is only meaningful in an IPC implementation.
}

static sourcekitd_response_receiver_t NotificationReceiver;

void
sourcekitd_set_notification_handler(sourcekitd_response_receiver_t receiver) {
  sourcekitd_response_receiver_t newReceiver = Block_copy(receiver);
  WorkQueue::dispatchOnMain([=]{
    Block_release(NotificationReceiver);
    NotificationReceiver = newReceiver;
  });
}

void sourcekitd::postNotification(sourcekitd_response_t Notification) {
  WorkQueue::dispatchOnMain([=]{
    if (!NotificationReceiver) {
      sourcekitd_response_dispose(Notification);
      return;
    }
    // The receiver accepts ownership of the notification object.
    NotificationReceiver(Notification);
  });
}
