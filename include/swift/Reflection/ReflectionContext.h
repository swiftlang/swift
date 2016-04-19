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
// Implements the context for allocations and management of structures related
// to reflection, such as TypeRefs.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_REFLECTIONCONTEXT_H
#define SWIFT_REFLECTION_REFLECTIONCONTEXT_H

#include "swift/Remote/MemoryReader.h"
#include "swift/Remote/MetadataReader.h"
#include "swift/Reflection/Records.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Reflection/TypeRefBuilder.h"
#include "swift/SwiftRemoteMirror/SwiftRemoteMirrorTypes.h"

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

  std::vector<std::pair<std::string, const TypeRef *>>
  getFieldTypeRefs(const TypeRef *TR) {
    TypeRefBuilder &Builder = getBuilder();
    auto *FD = Builder.getFieldTypeInfo(TR);
    if (FD == nullptr)
      return {};
    return Builder.getFieldTypeRefs(TR, FD);
  }

  std::vector<std::pair<std::string, const TypeRef *>>
  getFieldTypeRefs(StoredPointer MetadataAddress) {
    auto TR = readTypeFromMetadata(MetadataAddress);
    return getFieldTypeRefs(TR);
  }

  void addReflectionInfo(ReflectionInfo I) {
    getBuilder().addReflectionInfo(I);
  }

  swift_typeinfo_t getInfoForTypeRef(const TypeRef *TR) {
    // TODO
    return {
      SWIFT_UNKNOWN,
      NULL,
      0,
      0,
      0
    };
  }

  swift_childinfo_t getInfoForChild(const TypeRef *TR, unsigned Index) {
    // TODO
    return {
      0,
      NULL,
      0,
    };
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_REFLECTIONCONTEXT_H
