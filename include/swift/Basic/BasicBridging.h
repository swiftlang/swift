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

#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/StringRef.h"
#include <string>
#endif

// FIXME: We ought to be importing '<swift/briging>' instead.
#if __has_attribute(swift_name)
#define SWIFT_NAME(NAME) __attribute__((swift_name(NAME)))
#else
#define SWIFT_NAME(NAME)
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
  const char *_Nullable Data;
  size_t Length;

public:
#ifdef USED_IN_CPP_SOURCE
  BridgedStringRef(llvm::StringRef sref)
      : Data(sref.data()), Length(sref.size()) {}

  llvm::StringRef get() const { return llvm::StringRef(Data, Length); }
#endif

  SWIFT_NAME("init(data:count:)")
  BridgedStringRef(const char *_Nullable data, size_t length)
      : Data(data), Length(length) {}

  void write(BridgedOStream os) const;
};

SWIFT_NAME("getter:BridgedStringRef.data(self:)")
BRIDGED_INLINE 
const uint8_t *_Nullable BridgedStringRef_data(BridgedStringRef str);

SWIFT_NAME("getter:BridgedStringRef.count(self:)")
BRIDGED_INLINE SwiftInt BridgedStringRef_count(BridgedStringRef str);

SWIFT_NAME("getter:BridgedStringRef.isEmpty(self:)")
BRIDGED_INLINE bool BridgedStringRef_empty(BridgedStringRef str);

class BridgedOwnedString {
  char *_Nonnull Data;
  size_t Length;

public:
#ifdef USED_IN_CPP_SOURCE
  BridgedOwnedString(const std::string &stringToCopy);

  llvm::StringRef getRef() const { return llvm::StringRef(Data, Length); }
#endif

  void destroy() const;
};

SWIFT_NAME("getter:BridgedOwnedString.data(self:)")
BRIDGED_INLINE 
const uint8_t *_Nullable BridgedOwnedString_data(BridgedOwnedString str);

SWIFT_NAME("getter:BridgedOwnedString.count(self:)")
BRIDGED_INLINE SwiftInt BridgedOwnedString_count(BridgedOwnedString str);

SWIFT_NAME("getter:BridgedOwnedString.isEmpty(self:)")
BRIDGED_INLINE bool BridgedOwnedString_empty(BridgedOwnedString str);

class BridgedSourceLoc {
  const void *_Nullable Raw;

public:
  BridgedSourceLoc() : Raw(nullptr) {}

  SWIFT_NAME("init(raw:)")
  BridgedSourceLoc(const void *_Nullable raw) : Raw(raw) {}

#ifdef USED_IN_CPP_SOURCE
  BridgedSourceLoc(swift::SourceLoc loc) : Raw(loc.getOpaquePointerValue()) {}

  swift::SourceLoc get() const {
    return swift::SourceLoc(
        llvm::SMLoc::getFromPointer(static_cast<const char *>(Raw)));
  }
#endif

  SWIFT_IMPORT_UNSAFE
  const void *_Nullable getOpaquePointerValue() const { return Raw; }

  SWIFT_NAME("advanced(by:)")
  BRIDGED_INLINE
  BridgedSourceLoc advancedBy(size_t n) const;
};

SWIFT_NAME("getter:BridgedSourceLoc.isValid(self:)")
BRIDGED_INLINE bool BridgedSourceLoc_isValid(BridgedSourceLoc str);

struct BridgedArrayRef {
  const void * _Nullable data;
  size_t numElements;
};

SWIFT_END_NULLABILITY_ANNOTATIONS

#ifndef PURE_BRIDGING_MODE
// In _not_ PURE_BRIDGING_MODE, bridging functions are inlined and therefore
// included in the header file.
#include "BasicBridgingImpl.h"
#endif

#endif
