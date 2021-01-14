//===--- UIDHandling.cpp --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SourceKit/Support/UIdent.h"
#include "sourcekitd/Internal.h"

#include "llvm/Support/Mutex.h"

#include <Block.h>

using namespace SourceKit;

static llvm::sys::Mutex GlobalHandlersMtx;
static sourcekitd_uid_handler_t UidMappingHandler;
static sourcekitd_str_from_uid_handler_t StrFromUidMappingHandler;

void sourcekitd_set_uid_handler(sourcekitd_uid_handler_t handler) {
  llvm::sys::ScopedLock L(GlobalHandlersMtx);
  sourcekitd_uid_handler_t newHandler = Block_copy(handler);
  Block_release(UidMappingHandler);
  UidMappingHandler = newHandler;
}

void sourcekitd_set_uid_handlers(
    sourcekitd_uid_from_str_handler_t uid_from_str,
    sourcekitd_str_from_uid_handler_t str_from_uid) {
  llvm::sys::ScopedLock L(GlobalHandlersMtx);

  sourcekitd_uid_handler_t newUIDFromStrHandler = Block_copy(uid_from_str);
  Block_release(UidMappingHandler);
  UidMappingHandler = newUIDFromStrHandler;

  sourcekitd_str_from_uid_handler_t newStrFromUIDHandler =
      Block_copy(str_from_uid);
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
