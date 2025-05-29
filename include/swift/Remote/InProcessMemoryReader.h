//===--- InProcessMemoryReader.h - Access to local memory -------*- C++ -*-===//
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
//
//  This file declares an abstract interface for working with remote memory.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_INPROCESSMEMORYREADER_H
#define SWIFT_REMOTE_INPROCESSMEMORYREADER_H

#include "swift/Remote/MemoryReader.h"

#include <cstring>

#if defined(__APPLE__) && defined(__MACH__)
#include <TargetConditionals.h>
#endif

namespace swift {
namespace remote {

/// An implementation of MemoryReader which simply reads from the current
/// address space.
class InProcessMemoryReader final : public MemoryReader {
  bool queryDataLayout(DataLayoutQueryType type, void *inBuffer,
                       void *outBuffer) override {
#if defined(__APPLE__) && __APPLE__
    auto applePlatform = true;
#else
    auto applePlatform = false;
#endif
#if defined(__APPLE__) && __APPLE__ && ((defined(TARGET_OS_IOS) && TARGET_OS_IOS) || (defined(TARGET_OS_IOS) && TARGET_OS_WATCH) || (defined(TARGET_OS_TV) && TARGET_OS_TV) || defined(__arm64__))
    auto iosDerivedPlatform = true;
#else
    auto iosDerivedPlatform = false;
#endif

    switch (type) {
    case DLQ_GetPointerSize: {
      auto result = static_cast<uint8_t *>(outBuffer);
      *result = sizeof(void *);
      return true;
    }
    case DLQ_GetSizeSize: {
      auto result = static_cast<uint8_t *>(outBuffer);
      *result = sizeof(size_t);
      return true;
    }
    case DLQ_GetPtrAuthMask: {
      auto result = static_cast<uintptr_t *>(outBuffer);
#if __has_feature(ptrauth_calls)
      *result = (uintptr_t)ptrauth_strip((void*)0x0007ffffffffffff, 0);
#else
      *result = (uintptr_t)~0ull;
#endif
      return true;
    }
    case DLQ_GetObjCReservedLowBits: {
      auto result = static_cast<uint8_t *>(outBuffer);
      if (applePlatform && !iosDerivedPlatform && (sizeof(void *) == 8)) {
        // Obj-C reserves low bit on 64-bit Intel macOS only.
        // Other Apple platforms don't reserve this bit (even when
        // running on x86_64-based simulators).
        *result = 1;
      } else {
        *result = 0;
      }
      return true;
    }
    case DLQ_GetLeastValidPointerValue: {
      auto result = static_cast<uint64_t *>(outBuffer);
      if (applePlatform && (sizeof(void *) == 8)) {
        // Swift reserves the first 4GiB on Apple 64-bit platforms
        *result = 0x100000000;
        return 1;
      } else {
        // Swift reserves the first 4KiB everywhere else
        *result = 0x1000;
      }
      return true;
    }
    case DLQ_GetObjCInteropIsEnabled:
      break;
    }
    return false;
  }

  RemoteAddress getSymbolAddress(const std::string &name) override;

  bool readString(RemoteAddress address, std::string &dest) override {
    dest = address.getLocalPointer<char>();
    return true;
  }

  ReadBytesResult readBytes(RemoteAddress address, uint64_t size) override {
    return ReadBytesResult(address.getLocalPointer<void>(), [](const void *) {});
  }
};
 
}
}

#endif
