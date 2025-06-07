//===--- ExternalTypeRefCache.h - Abstract access to external caches of
//typeref ------*- C++ -*-===//
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
//
/// @file
/// This file declares an abstract interface for external caches of
/// typeref information.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_EXTERNALTYPEREFCACHE_H
#define SWIFT_REMOTE_EXTERNALTYPEREFCACHE_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include <optional>

#include <string>

namespace swift {
namespace reflection {

template <typename T>
class ReflectionSection;
class FieldDescriptorIterator;
using FieldSection = ReflectionSection<FieldDescriptorIterator>;
}

namespace remote {
/// A struct with the information required to locate a specific field
/// descriptor.
struct FieldDescriptorLocator {
  /// The reflection info ID the field descriptor belongs to.
  uint64_t InfoID;

  /// The offset of the field descriptor in the FieldSection buffer.
  uint64_t Offset;
};

/// An abstract interface for providing external type layout information.
struct ExternalTypeRefCache {
  virtual ~ExternalTypeRefCache() = default;

  /// Cache the field descriptors of a reflection info with a given id with
  /// their corresponding mangled names. The amount of field descriptors and
  /// mangled names must be the same. If a field descriptor does not have a
  /// mangled name a corresponding empty string must be in the mangled_names
  /// array.
  virtual void
  cacheFieldDescriptors(uint64_t InfoID,
                        const swift::reflection::FieldSection &FieldDescriptors,
                        llvm::ArrayRef<std::string> MangledNames) = 0;

  /// Retrieve a pair representing the reflection info id and the offset of a
  /// field descriptor in the field section buffer, if available.
  virtual std::optional<FieldDescriptorLocator>
  getFieldDescriptorLocator(const std::string &Name) = 0;

  /// Returns whether the reflection info with the corresponding ID has been
  /// cached already.
  virtual bool isReflectionInfoCached(uint64_t InfoID) = 0;
};

} // namespace remote
} // namespace swift
#endif
