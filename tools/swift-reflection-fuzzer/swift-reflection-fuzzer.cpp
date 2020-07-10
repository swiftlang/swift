//===--- swift-reflection-fuzzer.cpp - Swift reflection fuzzer ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This program tries to fuzz the metadata reader shipped as part of the swift
// runtime and used by the debugger.
// For this to work you need to pass --enable-sanitizer-coverage to build-script
// otherwise the fuzzer doesn't have coverage information to make progress
// (making the whole fuzzing operation really ineffective).
// It is recommended to use the tool together with another sanitizer to expose
// more bugs (asan, lsan, etc...)
//
//===----------------------------------------------------------------------===//

#include "swift/Reflection/ReflectionContext.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Reflection/TypeRefBuilder.h"
#include "llvm/Object/Archive.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFObjectFile.h"
#include "llvm/Object/MachOUniversal.h"
#include "llvm/Support/CommandLine.h"
#include <stddef.h>
#include <stdint.h>

#if defined(__APPLE__) && defined(__MACH__)
#include <TargetConditionals.h>
#endif

using namespace llvm::object;

using namespace swift;
using namespace swift::reflection;
using namespace swift::remote;

using NativeReflectionContext = swift::reflection::ReflectionContext<
    External<RuntimeTarget<sizeof(uintptr_t)>>>;

template <typename T> static T unwrap(llvm::Expected<T> value) {
  if (value)
    return std::move(value.get());
  return T();
}

class ObjectMemoryReader : public MemoryReader {
public:
  ObjectMemoryReader() {}

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
    case DLQ_GetObjCReservedLowBits: {
      auto result = static_cast<uint8_t *>(outBuffer);
      if (applePlatform && !iosDerivedPlatform && (sizeof(void *) == 8)) {
        // Obj-C reserves low bit on 64-bit macOS only.
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
    }

    return false;
  }

  RemoteAddress getSymbolAddress(const std::string &name) override {
    return RemoteAddress(nullptr);
  }

  bool isAddressValid(RemoteAddress addr, uint64_t size) const { return true; }

  ReadBytesResult readBytes(RemoteAddress address, uint64_t size) override {
    return ReadBytesResult((const void *)address.getAddressData(),
                           [](const void *) {});
  }

  bool readString(RemoteAddress address, std::string &dest) override {
    if (!isAddressValid(address, 1))
      return false;
    auto cString = StringRef((const char *)address.getAddressData());
    dest.append(cString.begin(), cString.end());
    return true;
  }
};

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t Size) {
  auto reader = std::make_shared<ObjectMemoryReader>();
  NativeReflectionContext context(std::move(reader));
  context.addImage(RemoteAddress(Data));
  context.getBuilder().dumpAllSections(std::cout);
  return 0; // Non-zero return values are reserved for future use.
}
