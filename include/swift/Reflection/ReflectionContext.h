//===--- ReflectionContext.h - Swift Type Reflection Context ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implements the context for reflection of values in the address space of a
// remote process.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_REFLECTIONCONTEXT_H
#define SWIFT_REFLECTION_REFLECTIONCONTEXT_H

#include "swift/Remote/MemoryReader.h"
#include "swift/Remote/MetadataReader.h"
#include "swift/Reflection/Records.h"
#include "swift/Reflection/TypeLowering.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Reflection/TypeRefBuilder.h"

#include <iostream>
#include <vector>
#include <unordered_map>

namespace swift {
namespace reflection {

using swift::remote::MemoryReader;
using swift::remote::RemoteAddress;

template <typename Runtime>
class ReflectionContext
       : public remote::MetadataReader<Runtime, TypeRefBuilder> {
  using super = remote::MetadataReader<Runtime, TypeRefBuilder>;

public:
  using super::getBuilder;
  using super::readTypeFromMetadata;
  using typename super::StoredPointer;

public:

  explicit ReflectionContext(std::shared_ptr<MemoryReader> reader)
    : super(std::move(reader)) {}

  ReflectionContext(const ReflectionContext &other) = delete;
  ReflectionContext &operator=(const ReflectionContext &other) = delete;

  MemoryReader &getReader() {
    return *this->Reader;
  }

  void dumpAllSections(std::ostream &OS) {
    getBuilder().dumpAllSections();
  }

  void addReflectionInfo(ReflectionInfo I) {
    getBuilder().addReflectionInfo(I);
  }

  /// Return a description of the layout of a heap object having the given
  /// metadata as its isa pointer.
  const TypeInfo *getTypeInfo(StoredPointer MetadataAddress) {
    // This is wrong...
    auto TR = readTypeFromMetadata(MetadataAddress);
    return getTypeInfo(TR);
  }

  /// Return a description of the layout of a value with the given type.
  const TypeInfo *getTypeInfo(const TypeRef *TR) {
    return getBuilder().getTypeConverter().getTypeInfo(TR);
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_REFLECTIONCONTEXT_H
