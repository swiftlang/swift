//===--- BasicBridging.h - header for the swift BasicBridging module ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_BASICBRIDGING_H
#define SWIFT_BASIC_BASICBRIDGING_H

#if !defined(COMPILED_WITH_SWIFT) || !defined(PURE_BRIDGING_MODE)
#define USED_IN_CPP_SOURCE
#endif

// Do not add other C++/llvm/swift header files here!
// Function implementations should be placed into BasicBridging.cpp and required header files should be added there.
//
#include "swift/Basic/BridgedSwiftObject.h"
#include "swift/Basic/Compiler.h"

#include <stddef.h>
#include <stdint.h>
#ifdef USED_IN_CPP_SOURCE
// Workaround to avoid a compiler error because `cas::ObjectRef` is not defined
// when including VirtualFileSystem.h
#include <cassert>
#include "llvm/CAS/CASReference.h"

#include "llvm/ADT/StringRef.h"
#include <string>
#endif

#ifdef PURE_BRIDGING_MODE
// In PURE_BRIDGING_MODE, briding functions are not inlined
#define BRIDGED_INLINE
#else
#define BRIDGED_INLINE inline
#endif

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

typedef intptr_t SwiftInt;
typedef uintptr_t SwiftUInt;

struct BridgedOStream {
  void * _Nonnull streamAddr;
};

class BridgedStringRef {
  const char * _Nonnull data;
  size_t length;

public:
#ifdef USED_IN_CPP_SOURCE
  BridgedStringRef(llvm::StringRef sref) : data(sref.data()), length(sref.size()) {}

  llvm::StringRef get() const { return llvm::StringRef(data, length); }
#endif

  BridgedStringRef(const char * _Nullable data, size_t length)
    : data(data), length(length) {}

  SWIFT_IMPORT_UNSAFE const uint8_t * _Nonnull uintData() const {
    return (const uint8_t * _Nonnull)data;
  }
  SwiftInt size() const { return (SwiftInt)length; }
  void write(BridgedOStream os) const;
};

class BridgedOwnedString {
  char * _Nonnull data;
  size_t length;

public:
#ifdef USED_IN_CPP_SOURCE
  BridgedOwnedString(const std::string &stringToCopy);
#endif

  SWIFT_IMPORT_UNSAFE const uint8_t * _Nonnull uintData() const {
    return (const uint8_t * _Nonnull)(data ? data : "");
  }
  SwiftInt size() const { return (SwiftInt)length; }
  void destroy() const;
};

class BridgedSourceLoc {
  const void * _Nullable opaquePointer;
public:
  BridgedSourceLoc() : opaquePointer(nullptr) {}
  BridgedSourceLoc(const void * _Nullable loc) : opaquePointer(loc) {}

  bool isValid() const { return opaquePointer != nullptr; }
  SWIFT_IMPORT_UNSAFE const uint8_t * _Nullable uint8Pointer() const {
    return (const uint8_t * _Nullable)opaquePointer;
  }
  const char * _Nullable getLoc() const {
    return (const char * _Nullable)opaquePointer;
  }
};

struct BridgedArrayRef {
  const void * _Nullable data;
  size_t numElements;
};

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif
